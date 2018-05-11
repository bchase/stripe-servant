{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Error where

import qualified Data.Text                 as T
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.Sequence             as Seq
import           Data.CaseInsensitive      as CI
import           GHC.Generics              (Generic)

import qualified Data.Aeson         as J
import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)
import           Servant.Client     (ServantError (..), Response (..))
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP



---- STRIPE ERROR TYPES ----

data StripeError = StripeError
  { errorStatusCode  :: Int
  , errorRequestId   :: String -- TODO RequestId
  , errorType        :: StripeErrorType
  , errorCode        :: Maybe StripeErrorCode
  , errorParam       :: Maybe String
  , errorMessage     :: Maybe String
  , errorCharge      :: Maybe String -- TODO ChargeId
  , errorDeclineCode :: Maybe String
  } deriving ( Show )

data StripeFailure
  = StripeErrorResponse StripeError
  | StripeDecodeFailure T.Text Response
  | StripeConnectionError T.Text
  deriving ( Show )

data StripeErrorType
  = ApiConnectionError
  | ApiError
  | AuthenticationError
  | CardError
  | IdempotencyError
  | InvalidRequestError
  | RateLimitError
  | ValidationError
  | UnrecognizedErrorType T.Text
  | ErrorJsonParseError Response
  deriving ( Show, Generic )

data StripeErrorCode
  = InvalidNumber
  | InvalidExpiryMonth
  | InvalidExpiryYear
  | InvalidCvc
  | InvalidSwipeData
  | IncorrectNumber
  | ExpiredCard
  | IncorrectCvc
  | IncorrectZip
  | CardDeclined
  | Missing
  | ProcessingError
  | UnrecognizedErrorCode T.Text
  deriving ( Show, Generic )

data StripeErrorJSON' = StripeErrorJSON'
  { errorJsonType        :: StripeErrorType
  , errorJsonCode        :: Maybe StripeErrorCode
  , errorJsonParam       :: Maybe String
  , errorJsonMessage     :: Maybe String
  , errorJsonCharge      :: Maybe String -- TODO ChargeId
  , errorJsonDeclineCode :: Maybe String
  } deriving ( Show, Generic )

data StripeErrorJSON = StripeErrorJSON
  { jsonError :: StripeErrorJSON'
  } deriving ( Show, Generic )


---- STRIPE ERROR TYPE INSTANCES ----

instance J.FromJSON StripeErrorCode where
  parseJSON (J.String "invalid_number")       = return InvalidNumber
  parseJSON (J.String "invalid_expiry_month") = return InvalidExpiryMonth
  parseJSON (J.String "invalid_expiry_year")  = return InvalidExpiryYear
  parseJSON (J.String "invalid_cvc")          = return InvalidCvc
  parseJSON (J.String "invalid_swipe_data")   = return InvalidSwipeData
  parseJSON (J.String "incorrect_number")     = return IncorrectNumber
  parseJSON (J.String "expired_card")         = return ExpiredCard
  parseJSON (J.String "incorrect_cvc")        = return IncorrectCvc
  parseJSON (J.String "incorrect_zip")        = return IncorrectZip
  parseJSON (J.String "card_declined")        = return CardDeclined
  parseJSON (J.String "missing")              = return Missing
  parseJSON (J.String "processing_error")     = return ProcessingError
  parseJSON (J.String txt)                    = return . UnrecognizedErrorCode $ txt
  parseJSON _                                 = return . UnrecognizedErrorCode $ ""

instance J.FromJSON StripeErrorType where
  parseJSON (J.String "api_connection_error")  = return ApiConnectionError
  parseJSON (J.String "api_error")             = return ApiError
  parseJSON (J.String "authentication_error")  = return AuthenticationError
  parseJSON (J.String "card_error")            = return CardError
  parseJSON (J.String "idempotency_error")     = return IdempotencyError
  parseJSON (J.String "invalid_request_error") = return InvalidRequestError
  parseJSON (J.String "rate_limit_error")      = return RateLimitError
  parseJSON (J.String "validation_error")      = return ValidationError
  parseJSON (J.String txt)                     = return . UnrecognizedErrorType $ txt
  parseJSON _                                  = return . UnrecognizedErrorType $ ""

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 9 } ''StripeErrorJSON')
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 4 } ''StripeErrorJSON)



---- STRIPE ERROR HELPERS ----

stripeError :: ServantError -> StripeFailure
stripeError servantErr =
  case servantErr of
    FailureResponse          resp     -> StripeErrorResponse . stripeErrFromResponse $ resp
    UnsupportedContentType _ resp     -> StripeErrorResponse . stripeErrFromResponse $ resp
    InvalidContentTypeHeader resp     -> StripeErrorResponse . stripeErrFromResponse $ resp
    DecodeFailure            err resp -> StripeDecodeFailure   err resp
    ConnectionError          err      -> StripeConnectionError err

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

    requestIdFromResp :: Response -> String -- TODO RequestId
    requestIdFromResp = maybe "" (C.unpack . snd) . getHeader "Request-Id" . responseHeaders

    getHeader :: B.ByteString -> Seq.Seq HTTP.Header -> Maybe HTTP.Header
    getHeader name = seqFind (\(hName, _)-> hName == CI.mk name)

    seqFind :: (a -> Bool) -> Seq.Seq a -> Maybe a
    seqFind chk ss = fmap (Seq.index ss) (Seq.findIndexL chk ss)



---- STRIPE ERROR EXPLANATIONS ----

class StripeErrorExplanation a where
  explain :: a -> String

instance StripeErrorExplanation StripeErrorType where
  explain ApiConnectionError  = "Failure to connect to Stripe's API."
  explain ApiError            = "API errors cover any other type of problem (e.g., a temporary problem with Stripe's servers) and are extremely uncommon."
  explain AuthenticationError = "Failure to properly authenticate yourself in the request."
  explain CardError           = "Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can't be charged for some reason."
  explain IdempotencyError    = "Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the API endpoint and parameters of the first."
  explain InvalidRequestError = "Invalid request errors arise when your request has invalid parameters."
  explain RateLimitError      = "Too many requests hit the API too quickly."
  explain ValidationError     = "Errors triggered by our client-side libraries when failing to validate fields (e.g., when a card number or expiration date is invalid or incomplete)."
  explain (ErrorJsonParseError _)   = "ErrorJsonParseError"
  explain (UnrecognizedErrorType t) = mconcat [ "UnrecognizedErrorType: \"", T.unpack t , "\"" ]

instance StripeErrorExplanation StripeErrorCode where
  explain InvalidNumber      = "The card number is not a valid credit card number."
  explain InvalidExpiryMonth = "The card's expiration month is invalid."
  explain InvalidExpiryYear  = "The card's expiration year is invalid."
  explain InvalidCvc         = "The card's security code is invalid."
  explain InvalidSwipeData   = "The card's swipe data is invalid."
  explain IncorrectNumber    = "The card number is incorrect."
  explain ExpiredCard        = "The card has expired."
  explain IncorrectCvc       = "The card's security code is incorrect."
  explain IncorrectZip       = "The card's zip code failed validation."
  explain CardDeclined       = "The card was declined."
  explain Missing            = "The re is no card on a customer that is being charged."
  explain ProcessingError    = "An error occurred while processing the card."
  explain (UnrecognizedErrorCode t) = mconcat [ "UnrecognizedErrorType: \"", T.unpack t , "\"" ]
