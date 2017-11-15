{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import qualified Data.Text               as T
import           Data.Proxy              (Proxy (Proxy))
import           GHC.Generics            (Generic)

import           Data.Aeson.Casing       (snakeCase)
import           Data.Aeson.TH           (Options (..), defaultOptions, deriveJSON)
import           Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import           Servant.API
import           Servant.Client          (ServantError, ClientM, Scheme (Https),
                                          ClientEnv (ClientEnv), BaseUrl (BaseUrl),
                                          runClientM, client)

import           Util                    (applyTo)


---- STRIPE DATA TYPES ----

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

---- API ENDPOINTS ----

type StripeAPI = ChargeList :<|> CustomerList

type ChargeList =
  "v1" :> "charges"
  :> Header "Stripe-Version" StripeVersion
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Account" StripeAccountId
  :> Get '[JSON] (Stripe [Charge])

type CustomerList =
  "v1" :> "customers"
  :> Header "Stripe-Version" StripeVersion
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Account" StripeAccountId
  :> Get '[JSON] (Stripe [Customer])

-- type StripeAPI    = "v1" :> StripeHeaders :> StripeAPI'
-- type StripeAPI'   = ChargeList :<|> CustomerList
-- type ChargeList   = "charges" :> GetS '[JSON] [Charge]
-- type CustomerList = "customers" :> GetS '[JSON] [Customers]

-- -- TODO possible ?
-- type StripeAPI =
--   Header "Authorization"  StripeSecretKey :>
--   Header "Stripe-Version" StripeVersion :>
--   Header "Stripe-Account" StripeAccountId :>
--   "v1" :> StripeAPI'
--
-- type StripeAPI' =
--        "charges" :> Get '[JSON] (Stripe Charge)
--   :<|> "customers" :> Get '[JSON] (Stripe Customer)

---- ENDPOINT FUNCS ----

-- getCharges :: Maybe StripeSecretKey -> Maybe StripeVersion -> Maybe StripeAccountId -> ClientM (Stripe Charge)
-- getCustomers :: Maybe StripeSecretKey -> Maybe StripeVersion -> Maybe StripeAccountId -> ClientM (Stripe Customer)
-- getCharges :<|> getCustomers = client (Proxy :: Proxy StripeAPI)

getCharges :: Maybe StripeAccountId -> ClientM (Stripe [Charge])
getCustomers :: Maybe StripeAccountId -> ClientM (Stripe [Customer])
getCharges :<|> getCustomers = buildClientFuncs
  where
    buildClientFuncs = applyTo (Just secretKey) . applyTo (Just stripeVer) . client $ api
    api       = Proxy :: Proxy StripeAPI
    stripeVer = StripeVersion'2017'08'15
    secretKey = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"


-- TODO
--   * flesh out data types
--   * add endpoints
--   * errors
--   * pagination
--   * metadata
--   ? request IDs
--   ? idempotency

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

---- GENERAL STRIPE DATA TYPES ----

data Stripe a = Stripe
  { stripeObject  :: String
  , stripeUrl     :: String
  , stripeHasMore :: Bool
  , stripeData    :: a
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 6 } ''Stripe)

-- type ResourceId = String
-- data Pagination = Pagination
--   { pLimit         :: Maybe Int
--   , pStartingAfter :: Maybe ResourceId
--   , pEndingBefore  :: Maybe ResourceId
--   }
-- pagination :: Pagination
-- pagination = Pagination Nothing Nothing Nothing

-- data StripeConnect
--   = WithoutConnect
--   | WithConnect StripeAccountId
-- stripeConnectToMaybe :: StripeConnect -> Maybe StripeAccountId
-- stripeConnectToMaybe s =
--   case s of
--     WithoutConnect  -> Nothing
--     WithConnect id' -> Just id'


---- CLIENT RUNNER ----

data StripeClient a = ClientM (Stripe a)

stripe :: Maybe StripeAccountId -> (Maybe StripeAccountId -> ClientM a) -> IO (Either ServantError a)
stripe mAccountId clientM = do
  clientEnv >>= runClientM (clientM mAccountId)
  where
    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let basePath = ""
          url = BaseUrl Https "api.stripe.com" 443 basePath
      return $ ClientEnv manager url
