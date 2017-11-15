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

type StripeAPI = "v1" :> (ChargeList :<|> CustomerList)

type ChargeList =
  "charges"
  :> Header "Stripe-Version" StripeVersion
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Account" StripeAccountId
  :> Get '[JSON] (StripeResp [Charge])

type CustomerList =
  "customers"
  :> Header "Stripe-Version" StripeVersion
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Account" StripeAccountId
  :> Get '[JSON] (StripeResp [Customer])

-- -- TODO possible ?
-- type StripeAPI =
--   Header "Authorization"  StripeSecretKey :>
--   Header "Stripe-Version" StripeVersion :>
--   Header "Stripe-Account" StripeAccountId :>
--   "v1" :> StripeAPI'
--
-- type StripeAPI' =
--        "charges" :> Get '[JSON] (StripeResp Charge)
--   :<|> "customers" :> Get '[JSON] (StripeResp Customer)

---- ENDPOINT FUNCS ----

-- getCharges :: Maybe StripeSecretKey -> Maybe StripeVersion -> Maybe StripeAccountId -> ClientM (StripeResp Charge)
-- getCustomers :: Maybe StripeSecretKey -> Maybe StripeVersion -> Maybe StripeAccountId -> ClientM (StripeResp Customer)
-- getCharges :<|> getCustomers = client (Proxy :: Proxy StripeAPI)

type StripeConnect'  = Maybe StripeAccountId
type StripeClient' a = StripeConnect' -> StripeClient a
type StripeClient  a = ClientM (StripeResp a)

getCharges :: StripeConnect' -> StripeClient [Charge]
getCustomers :: StripeConnect' -> StripeClient [Customer]
getCharges :<|> getCustomers = buildClientFuncs
  where
    buildClientFuncs = applyTo (Just secretKey) . applyTo (Just stripeVer) . client $ api
    api       = Proxy :: Proxy StripeAPI
    stripeVer = StripeVersion'2017'08'15
    secretKey = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"


-- TODO
--   * errors
--   * pagination
--   * metadata
--   ! flesh out data types
--   ! (define needed and) add endpoints
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

data StripeResp a = StripeResp
  { stripeRespObject  :: String
  , stripeRespUrl     :: String
  , stripeRespHasMore :: Bool
  , stripeRespData    :: a
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 10 } ''StripeResp)

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

connectToMaybe :: StripeConnect -> Maybe StripeAccountId
connectToMaybe s =
  case s of
    WithoutConnect  -> Nothing
    WithConnect id' -> Just id'

-- type ResourceId = String
-- data Pagination = Pagination
--   { pLimit         :: Maybe Int
--   , pStartingAfter :: Maybe ResourceId
--   , pEndingBefore  :: Maybe ResourceId
--   }
-- pagination :: Pagination
-- pagination = Pagination Nothing Nothing Nothing



---- CLIENT RUNNER ----

stripe :: StripeConnect -> StripeClient' a -> IO (Either ServantError (StripeResp a))
stripe connect clientM = clientEnv >>= runClientM (clientM . connectToMaybe $ connect)
  where
    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let basePath = ""
          url = BaseUrl Https "api.stripe.com" 443 basePath
      return $ ClientEnv manager url
