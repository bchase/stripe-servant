{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Types where

import qualified Data.Text          as T
import           GHC.Generics       (Generic)

import qualified Data.Aeson         as J
import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)
import           Servant.API
import           Servant.Client     (ClientM, Response (..))


type RequestId = String

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

newtype ResourceId = ResourceId String deriving (Show, Generic, J.FromJSON)


-- -- PAGINATION -- --

data PaginationOpt
  = PaginateBy Int
  | PaginateStartingAfter ResourceId
  | PaginateEndingBefore ResourceId

newtype PaginationLimit         = PaginateBy'            Int        deriving (Generic)
newtype PaginationStartingAfter = PaginateStartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = PaginateEndingBefore'  ResourceId deriving (Generic)

data PaginationOpts = PaginationOpts
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

instance ToHttpApiData PaginationLimit where
  toUrlPiece (PaginateBy' num) = T.pack . show $ num
instance ToHttpApiData PaginationStartingAfter where
  toUrlPiece (PaginateStartingAfter' (ResourceId id')) = T.pack . show $ id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (PaginateEndingBefore' (ResourceId id')) = T.pack . show $ id'


-- -- HEADER TYPES -- --

newtype StripeSecretKey = StripeSecretKey String
newtype StripeAccountId = StripeAccountId String
data    StripeVersion   = StripeVersion'2017'08'15

instance ToHttpApiData StripeSecretKey where
  toUrlPiece (StripeSecretKey key) = T.pack . mconcat $ [ "Bearer ", key ] -- TODO ++
instance ToHttpApiData StripeAccountId where
  toUrlPiece (StripeAccountId id') = T.pack id' -- TODO convert to {...} and `un*`
instance ToHttpApiData StripeVersion where
  toUrlPiece StripeVersion'2017'08'15 = "2017-08-15"


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
  , errorJsonCharge      :: Maybe ResourceId -- TODO ChargeId
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
  , errorCharge      :: Maybe ResourceId -- TODO ChargeId
  , errorDeclineCode :: Maybe String
  } deriving (Show)

data StripeFailure
  = StripeErrorResponse StripeError
  | StripeDecodeFailure T.Text Response
  | StripeConnectionError T.Text
  deriving (Show)


-- -- RESPONSES -- --

type Stripe  a  = Either StripeFailure a
type StripeS a  = Stripe (StripeScalar  a)
type StripeL a  = Stripe (StripeList    a)
type StripeD id = Stripe (StripeDelete id)

type StripeScalarResp a  = Headers '[Header "Request-Id" String]                    a
type StripeListResp   a  = Headers '[Header "Request-Id" String] (StripeListJSON    a)
type StripeDeleteResp id = Headers '[Header "Request-Id" String] (StripeDeleteJSON id)

data StripeScalar a = StripeScalar
  { stripeRequestId :: RequestId
  , stripeData      :: a
  } deriving (Show, Generic)

data StripeList a = StripeList
  { stripeListRequestId :: RequestId
  , stripeListHasMore   :: Bool
  , stripeListData      :: a
  } deriving (Show, Generic)

data StripeDelete id = StripeDelete
  { stripeDeleteRequestId :: RequestId
  , stripeDeleteId        :: id
  , stripeDeleteDeleted   :: Bool
  } deriving (Show, Generic)

data StripeListJSON a = StripeListJSON
  { stripeListJsonObject  :: String
  , stripeListJsonUrl     :: String
  , stripeListJsonHasMore :: Bool
  , stripeListJsonData    :: a
  } deriving (Show, Generic)

data StripeDeleteJSON id = StripeDeleteJSON
  { stripeDeleteJsonId      :: id
  , stripeDeleteJsonDeleted :: Bool
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''StripeListJSON)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 16 } ''StripeDeleteJSON)


-- -- CLIENTS -- --

type RunnableStripeClient a = ClientM a

type StripeClient resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> RunnableStripeClient resp

type StripeListClient resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> StripeClient (StripeListResp resp)
