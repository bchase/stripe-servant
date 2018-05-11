{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe.Types where

import qualified Data.Text            as T
import           Data.Char            (toLower)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT)
import           Control.Monad.Except (MonadError, ExceptT, runExceptT)
import           GHC.Generics         (Generic)

import qualified Data.Aeson         as J
import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)
import           Servant.API
import           Servant.Client     (ClientM, Response (..))



-- -- GENERIC IDS -- --

type RequestId = String

newtype ResourceId = ResourceId T.Text deriving (Show, Generic, J.FromJSON)



-- -- PAGINATION -- --

data PaginationOpt
  = By            Int
  | StartingAfter ResourceId
  | EndingBefore  ResourceId

newtype PaginationLimit         = By'            Int        deriving (Generic)
newtype PaginationStartingAfter = StartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = EndingBefore'  ResourceId deriving (Generic)

data PaginationOpts = PaginationOpts
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

instance ToHttpApiData PaginationLimit where
  toUrlPiece (By' num) = T.pack . show $ num
instance ToHttpApiData PaginationStartingAfter where
  toUrlPiece (StartingAfter' (ResourceId id')) = T.pack . show $ id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (EndingBefore' (ResourceId id')) = T.pack . show $ id'



-- -- HEADER TYPES -- --

newtype StripeSecretKey = StripeSecretKey { unStripeSecretKey :: T.Text }
newtype StripeAccountId = StripeAccountId { unStripeAccountId :: T.Text }
data    StripeVersion   = StripeVersion'2017'08'15

instance ToHttpApiData StripeSecretKey where
  toUrlPiece = mappend "Bearer " . unStripeSecretKey
instance ToHttpApiData StripeAccountId where
  toUrlPiece = unStripeAccountId
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



-- -- `Stripe` MONAD -- --

data StripeConfig = StripeConfig
  { stripeVersion   :: StripeVersion
  , stripeSecretKey :: StripeSecretKey
  }

newtype Stripe a = Stripe { runStripe :: ReaderT StripeConfig ( ExceptT StripeFailure IO ) a }
  deriving ( Functor, Applicative, Monad, MonadReader StripeConfig, MonadError StripeFailure, MonadIO )

stripeIO :: StripeConfig -> Stripe a -> IO (Either StripeFailure a)
stripeIO cfg = runExceptT . flip runReaderT cfg . runStripe



-- -- RESPONSES -- --

type StripeScalarResp   a = Headers '[Header "Request-Id" String]                    a
type StripeListResp     a = Headers '[Header "Request-Id" String] (StripeListJSON    a)
type StripeDestroyResp id = Headers '[Header "Request-Id" String] (StripeDeleteJSON id)

data StripeScalar a = StripeScalar
  { stripeScalarRequestId :: RequestId
  , stripeScalarData      :: a
  } deriving ( Show, Generic, Functor )

data StripeList a = StripeList
  { stripeListRequestId :: RequestId
  , stripeListHasMore   :: Bool
  , stripeListData      :: a
  } deriving ( Show, Generic, Functor )

data StripeDestroy id = StripeDestroy
  { stripeDestroyRequestId :: RequestId
  , stripeDestroyId        :: id
  , stripeDestroyDeleted   :: Bool
  } deriving ( Show, Generic, Functor )

data StripeListJSON a = StripeListJSON
  { stripeListJsonObject  :: String
  , stripeListJsonUrl     :: String
  , stripeListJsonHasMore :: Bool
  , stripeListJsonData    :: a
  } deriving ( Show, Generic, Functor )

data StripeDeleteJSON id = StripeDeleteJSON
  { stripeDeleteJsonId      :: id
  , stripeDeleteJsonDeleted :: Bool
  } deriving ( Show, Generic, Functor )

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''StripeListJSON)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 16 } ''StripeDeleteJSON)


class StripeData s where
  stripeReqId :: s a -> RequestId
  stripeData  :: s a -> a

instance StripeData StripeScalar where
  stripeReqId = stripeScalarRequestId
  stripeData  = stripeScalarData
instance StripeData StripeList where
  stripeReqId = stripeListRequestId
  stripeData  = stripeListData
instance StripeData StripeDestroy where
  stripeReqId = stripeDestroyRequestId
  stripeData  = stripeDestroyId



-- -- CLIENTS -- --

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

type RunnableStripeClient a = ClientM a

type StripeClient resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> RunnableStripeClient resp

type StripeClientPaginated resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> StripeClient resp



-- -- API TYPE / FUNC HELPERS -- --

type CapId t = Capture "id" t
type RBody t = ReqBody '[FormUrlEncoded] t

type GetListS a = Get    '[JSON] (StripeListResp    a)
type GetShowS a = Get    '[JSON] (StripeScalarResp  a)
type PostS    a = Post   '[JSON] (StripeScalarResp  a)
type DeleteS  a = Delete '[JSON] (StripeDestroyResp a)

type StripeHeaders resp =
     Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> resp

type StripePaginationQueryParams resp =
     QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> resp


type ListS           resp =              StripeClientPaginated (StripeListResp    resp)
type CreateS     req resp =       req -> StripeClient          (StripeScalarResp  resp)
type UpdateS  id req resp = id -> req -> StripeClient          (StripeScalarResp  resp)
type ReadS    id     resp = id ->        StripeClient          (StripeScalarResp  resp)
type DestroyS id          = id ->        StripeClient          (StripeDestroyResp   id)



-- -- CODES (e.g. COUNTRY, CURRENCY) -- --

data CountryCode
  = US
  | UnrecognizedCountryCode T.Text
  deriving (Show, Generic)

instance J.FromJSON CountryCode where
  parseJSON (J.String "US") = return US
  parseJSON (J.String str)  = return . UnrecognizedCountryCode $ str
  parseJSON _ = mempty

instance ToHttpApiData CountryCode where
  toQueryParam (UnrecognizedCountryCode code) = code
  toQueryParam code = T.pack . show $ code


data CurrencyCode -- ISO4217
  = USD
  | JPY
  | UnrecognizedCurrencyCode T.Text
  deriving (Show, Generic)

instance J.FromJSON CurrencyCode where
  parseJSON (J.String "usd") = return USD
  parseJSON (J.String "jpy") = return JPY
  parseJSON (J.String str)   = return . UnrecognizedCurrencyCode $ str
  parseJSON _ = mempty

instance ToHttpApiData CurrencyCode where
  toQueryParam (UnrecognizedCurrencyCode code) = code
  toQueryParam code = T.pack . map toLower . show $ code



-- -- PRICES -- --

data Price = Price
  { stripeCurrency :: CurrencyCode
  , stripeAmount   :: Int
  }

instance Show Price where
  show (Price (UnrecognizedCurrencyCode curr) amount) =
    "$" ++ show amount ++ " (" ++ T.unpack curr ++ ")"
  show (Price USD amount) = "$" ++ show amount
  show (Price JPY amount) = "¥" ++ show amount
