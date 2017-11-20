{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Stripe.Types where

import qualified Data.Text          as T
import           GHC.Generics       (Generic)

import qualified Data.Aeson         as J
import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)
import           Servant.Client     (Response (..))


type RequestId = String -- TODO newtype ?

newtype ResourceId = ResourceId String
  deriving (Show, Generic, J.FromJSON)

-- -- ERRORS -- --

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
  deriving (Show, Generic)

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
  deriving (Show, Generic)

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


data StripeErrorJSON' = StripeErrorJSON'
  { errorJsonType        :: StripeErrorType
  , errorJsonCode        :: Maybe StripeErrorCode
  , errorJsonParam       :: Maybe String
  , errorJsonMessage     :: Maybe String
  , errorJsonCharge      :: Maybe ResourceId
  , errorJsonDeclineCode :: Maybe String
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 9 } ''StripeErrorJSON')

data StripeErrorJSON = StripeErrorJSON
  { jsonError :: StripeErrorJSON'
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 4 } ''StripeErrorJSON)

data StripeError = StripeError
  { errorStatusCode  :: Int
  , errorRequestId   :: RequestId
  , errorType        :: StripeErrorType
  , errorCode        :: Maybe StripeErrorCode
  , errorParam       :: Maybe String
  , errorMessage     :: Maybe String
  , errorCharge      :: Maybe ResourceId
  , errorDeclineCode :: Maybe String
  } deriving (Show)

data StripeFailure
  = StripeErrorResponse StripeError
  | StripeDecodeFailure T.Text Response
  | StripeConnectionError T.Text
  deriving (Show)

