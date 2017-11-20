{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Error ( stripeError ) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.Sequence             as Seq
import           Data.CaseInsensitive      as CI

import qualified Data.Aeson                as J
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import           Servant.Client            (ServantError (..), Response (..))

import           Stripe.Types


stripeError :: ServantError -> StripeFailure
stripeError err =
  case err of
    FailureResponse resp             -> StripeErrorResponse . stripeErrFromResponse $ resp
    UnsupportedContentType _ resp    -> StripeErrorResponse . stripeErrFromResponse $ resp
    InvalidContentTypeHeader resp    -> StripeErrorResponse . stripeErrFromResponse $ resp
    DecodeFailure errTxt resp        -> StripeDecodeFailure errTxt resp
    ConnectionError httpExceptionTxt -> StripeConnectionError httpExceptionTxt

  where
    stripeErrFromResponse :: Response -> StripeError
    stripeErrFromResponse resp =
      case errorJsonFromResp resp of
        Just json ->
          StripeError
            { errorStatusCode  = statusCodeFromResp   resp
            , errorRequestId   = requestIdFromResp    resp
            , errorType        = errorJsonType        . jsonError $ json
            , errorCode        = errorJsonCode        . jsonError $ json
            , errorParam       = errorJsonParam       . jsonError $ json
            , errorMessage     = errorJsonMessage     . jsonError $ json
            , errorCharge      = errorJsonCharge      . jsonError $ json
            , errorDeclineCode = errorJsonDeclineCode . jsonError $ json
            }
        Nothing ->
          StripeError
            { errorStatusCode  = statusCodeFromResp resp
            , errorRequestId   = requestIdFromResp resp
            , errorType        = ErrorJsonParseError resp
            , errorCode        = Nothing
            , errorParam       = Nothing
            , errorMessage     = Nothing
            , errorCharge      = Nothing
            , errorDeclineCode = Nothing
            }
    errorJsonFromResp :: Response -> Maybe StripeErrorJSON
    errorJsonFromResp = J.decode . responseBody

    statusCodeFromResp :: Response -> Int
    statusCodeFromResp = HTTP.statusCode . responseStatusCode

    requestIdFromResp :: Response -> RequestId
    requestIdFromResp = maybe "" (C.unpack . snd) . getHeader "Request-Id" . responseHeaders

    getHeader :: B.ByteString -> Seq.Seq HTTP.Header -> Maybe HTTP.Header
    getHeader name = seqFind (\(hName, _)-> hName == CI.mk name)

    seqFind :: (a -> Bool) -> Seq.Seq a -> Maybe a
    seqFind chk ss = fmap (Seq.index ss) (Seq.findIndexL chk ss)



-- class HasExplanation a where
--   explain :: a -> String
--
-- instance HasExplanation StripeErrorType where
--   explain ApiConnectionError  = "Failure to connect to Stripe's API."
--   explain ApiError            = "API errors cover any other type of problem (e.g., a temporary problem with Stripe's servers) and are extremely uncommon."
--   explain AuthenticationError = "Failure to properly authenticate yourself in the request."
--   explain CardError           = "Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can't be charged for some reason."
--   explain IdempotencyError    = "Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the API endpoint and parameters of the first."
--   explain InvalidRequestError = "Invalid request errors arise when your request has invalid parameters."
--   explain RateLimitError      = "Too many requests hit the API too quickly."
--   explain ValidationError     = "Errors triggered by our client-side libraries when failing to validate fields (e.g., when a card number or expiration date is invalid or incomplete)."
--   explain (ErrorJsonParseError _)   = "ErrorJsonParseError"
--   explain (UnrecognizedErrorType t) = mconcat [ "UnrecognizedErrorType: \"", T.unpack t , "\"" ]
--
-- instance HasExplanation StripeErrorCode where
--   explain InvalidNumber      = "The card number is not a valid credit card number."
--   explain InvalidExpiryMonth = "The card's expiration month is invalid."
--   explain InvalidExpiryYear  = "The card's expiration year is invalid."
--   explain InvalidCvc         = "The card's security code is invalid."
--   explain InvalidSwipeData   = "The card's swipe data is invalid."
--   explain IncorrectNumber    = "The card number is incorrect."
--   explain ExpiredCard        = "The card has expired."
--   explain IncorrectCvc       = "The card's security code is incorrect."
--   explain IncorrectZip       = "The card's zip code failed validation."
--   explain CardDeclined       = "The card was declined."
--   explain Missing            = "The re is no card on a customer that is being charged."
--   explain ProcessingError    = "An error occurred while processing the card."
--   explain (UnrecognizedErrorCode t) = mconcat [ "UnrecognizedErrorType: \"", T.unpack t , "\"" ]
