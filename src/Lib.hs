{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import qualified Data.Text                 as T
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.Sequence             as Seq
import           Data.CaseInsensitive      as CI
import           Data.Maybe                (maybe)
import           Data.Proxy                (Proxy (Proxy))
import           GHC.Generics              (Generic)

import qualified Data.Aeson                as J
import           Data.Aeson.Casing         (snakeCase)
import           Data.Aeson.TH             (Options (..), defaultOptions, deriveJSON, deriveFromJSON)
import           Network.HTTP.Client.TLS   (newTlsManagerWith, tlsManagerSettings)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import           Servant.API
import           Servant.Client            (ServantError (..), ClientM, ClientEnv (ClientEnv),
                                            Response (..), Scheme (Https), BaseUrl (BaseUrl),
                                            runClientM, client)


-- { "error":
--   { "type": "invalid_request_error"    -- ErrorType
--   , "message": "Invalid integer: asdf" -- String
--   , "param": "amount"                  -- Param
--   }
-- }
--
-- TODO
--   * errors
-- X * pagination
--   * metadata
-- X ? request IDs
--   ? idempotency
--   ! flesh out data types
--   ! (define needed and) add endpoints
--   - mv things to Stripe.Client/Stripe.Data/etc.
--   - Persistent-style TH for type definitions/JSON
--   - change `String` to `Text`


---- PAGINATION ----

---- PUBLIC PAGINATION ----

newtype ResourceId = ResourceId String
  deriving (Show, Generic, J.FromJSON)

data Pagination
  = PaginateBy Int -- Nat
  | PaginateStartingAfter ResourceId
  | PaginateEndingBefore ResourceId

---- PRIVATE PAGINATION ----

newtype PaginationLimit         = PaginateBy' Int                   deriving (Generic)
newtype PaginationStartingAfter = PaginateStartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = PaginateEndingBefore' ResourceId  deriving (Generic)

data Pagination' = Pagination'
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

buildPagination :: [Pagination] -> Pagination'
buildPagination = foldl updatePagination emptyPagination
  where
    emptyPagination = Pagination' Nothing Nothing Nothing
    updatePagination p' p =
      case p of
        PaginateBy num            -> p' { paginateBy            = Just . PaginateBy'            $ num }
        PaginateStartingAfter id' -> p' { paginateStartingAfter = Just . PaginateStartingAfter' $ id' }
        PaginateEndingBefore id'  -> p' { paginateEndingBefore  = Just . PaginateEndingBefore'  $ id' }

instance ToHttpApiData PaginationLimit where
  toUrlPiece (PaginateBy' num) = T.pack . show $ num
instance ToHttpApiData PaginationStartingAfter where
  toUrlPiece (PaginateStartingAfter' (ResourceId id')) = T.pack . show $ id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (PaginateEndingBefore' (ResourceId id')) = T.pack . show $ id'

---- END PAGINATION ----



---- STRIPE API DATA TYPES ----

data Charge = Charge
  { chargeId       :: String
  , chargeAmount   :: Int
  , chargeCurrency :: String
  } deriving (Show, Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 6 } ''Charge)

data Customer = Customer
  { customerId    :: String
  , customerEmail :: Maybe String
  } deriving (Show, Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 8 } ''Customer)

---- STRIPE API ENDPOINTS ----

type StripeAPI = "v1" :> (ChargeList :<|> CustomerList)

type StripeRoute route =
     Header     "Stripe-Version" StripeVersion
  :> Header     "Authorization"  StripeSecretKey
  :> Header     "Stripe-Account" StripeAccountId
  :> QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> route

type ChargeList = StripeRoute ("charges" :> Get '[JSON] (StripeResp [Charge]))
type CustomerList = StripeRoute ("customers" :> Get '[JSON] (StripeResp [Customer]))

---- STRIPE ENDPOINT FUNCS ----

type StripeClient a =
     Maybe StripeVersion
  -> Maybe StripeSecretKey
  -> Maybe StripeAccountId
  -> Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> ClientM (StripeResp a)

getCharges :: StripeClient [Charge]
getCustomers :: StripeClient [Customer]
getCharges :<|> getCustomers = client (Proxy :: Proxy StripeAPI)


---- GENERAL STRIPE DATA TYPES ----

---- HEADER TYPES ----

newtype StripeSecretKey = StripeSecretKey String
newtype StripeAccountId = StripeAccountId String
data StripeVersion = StripeVersion'2017'08'15

instance ToHttpApiData StripeSecretKey where
  toUrlPiece (StripeSecretKey key) = T.pack . mconcat $ [ "Bearer ", key ]
instance ToHttpApiData StripeAccountId where
  toUrlPiece (StripeAccountId id') = T.pack id'
instance ToHttpApiData StripeVersion where
  toUrlPiece StripeVersion'2017'08'15 = "2017-08-15"


type RequestId = String -- TODO newtype ?
type StripeResp a = Headers '[Header "Request-Id" String] (StripeJSON a)
data Stripe a = Stripe
  { stripeRequestId :: RequestId
  , stripeJson      :: StripeJSON a
  } deriving (Show, Generic)

data StripeJSON a = StripeJSON
  { stripeJsonObject  :: String
  , stripeJsonUrl     :: String
  , stripeJsonHasMore :: Bool
  , stripeJsonData    :: a
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 10 } ''StripeJSON)

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId



---- CLIENT RUNNER ----


-- stripe :: StripeConnect -> [Pagination] -> StripeClient a -> IO (Either Error (Stripe a))
stripe :: StripeConnect -> [Pagination] -> StripeClient a -> IO (Either StripeFailure (Stripe a))
stripe connect pagination clientM = clientEnv >>= runClientM clientM' >>= buildStripe
  where
    clientM' =
      clientM
        (Just StripeVersion'2017'08'15)
        (Just . StripeSecretKey $ "sk_test_BQokikJOvBiI2HlWgH4olfQ2")
        (connectToMaybe connect)
        paginateBy
        paginateStartingAfter
        paginateEndingBefore
      where Pagination'{..} = buildPagination pagination

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url

    connectToMaybe s =
      case s of
        WithoutConnect  -> Nothing
        WithConnect id' -> Just id'

    -- buildStripe :: Either ServantError (StripeResp a) -> IO (Either Error (Stripe a))
    buildStripe :: Either ServantError (StripeResp a) -> IO (Either StripeFailure (Stripe a))
    buildStripe eResp =
      case eResp of
        Right resp -> return . Right . stripeFromResp $ resp
        -- Left  err  -> return . Left . Error $ err
        Left  err  -> return . Left . stripeError . Error $ err
      where
        stripeFromResp :: StripeResp a -> Stripe a
        stripeFromResp (Headers a hs) = Stripe (mReqId hs) a

        mReqId :: HList '[Header "Request-Id" String] -> String
        mReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
        mReqId _ = ""



-- explType :: StripeErrorType -> String
-- explType ApiConnectionError  = "Failure to connect to Stripe's API."
-- explType ApiError            = "API errors cover any other type of problem (e.g., a temporary problem with Stripe's servers) and are extremely uncommon."
-- explType AuthenticationError = "Failure to properly authenticate yourself in the request."
-- explType CardError           = "Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can't be charged for some reason."
-- explType IdempotencyError    = "Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the API endpoint and parameters of the first."
-- explType InvalidRequestError = "Invalid request errors arise when your request has invalid parameters."
-- explType RateLimitError      = "Too many requests hit the API too quickly."
-- explType ValidationError     = "Errors triggered by our client-side libraries when failing to validate fields (e.g., when a card number or expiration date is invalid or incomplete)."
-- explType (UnrecognizedErrorType t) = t -- mconcat [ "[[stripe-client]] could not parse the following error type: \"", t , "\"" ]

-- explCode :: StripeErrorCode -> String
-- explCode InvalidNumber      = "The card number is not a valid credit card number."
-- explCode InvalidExpiryMonth = "The card's expiration month is invalid."
-- explCode InvalidExpiryYear  = "The card's expiration year is invalid."
-- explCode InvalidCvc         = "The card's security code is invalid."
-- explCode InvalidSwipeData   = "The card's swipe data is invalid."
-- explCode IncorrectNumber    = "The card number is incorrect."
-- explCode ExpiredCard        = "The card has expired."
-- explCode IncorrectCvc       = "The card's security code is incorrect."
-- explCode IncorrectZip       = "The card's zip code failed validation."
-- explCode CardDeclined       = "The card was declined."
-- explCode Missing            = "The re is no card on a customer that is being charged."
-- explCode ProcessingError    = "An error occurred while processing the card."
