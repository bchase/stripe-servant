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

module Stripe where

import qualified Data.Text                 as T
import           Data.Proxy                (Proxy (Proxy))
import           GHC.Generics              (Generic)

import           Data.Aeson.Casing         (snakeCase)
import           Data.Aeson.TH             (Options (..), defaultOptions, deriveFromJSON)
import           Network.HTTP.Client.TLS   (newTlsManagerWith, tlsManagerSettings)
import           Servant.API
import           Servant.Client            (ClientM, ClientEnv (ClientEnv),
                                            Scheme (Https), BaseUrl (BaseUrl),
                                            ServantError (..), runClientM, client)

import           Stripe.Types              (ResourceId (ResourceId), RequestId)
import           Stripe.Error              (StripeFailure (..), stripeError)


-- TODO
-- \ * errors
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
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 6 } ''Charge)

data Customer = Customer
  { customerId    :: String
  , customerEmail :: Maybe String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 8 } ''Customer)

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

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 10 } ''StripeJSON)

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
        Left  err  -> return . Left . stripeError $ err
      where
        stripeFromResp :: StripeResp a -> Stripe a
        stripeFromResp (Headers a hs) = Stripe (mReqId hs) a

        mReqId :: HList '[Header "Request-Id" String] -> String
        mReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
        mReqId _ = ""
