{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Stripe
  ( module Stripe
  , module Stripe.Types
  , module Stripe.Helpers
  ) where

import           Prelude                     hiding (ReadS)
import qualified Data.Text                   as T
import           Data.Proxy                  (Proxy (Proxy))
import           GHC.Generics                (Generic)

import           Data.Aeson                  as J
import           Data.Aeson.Casing           (snakeCase)
import           Data.Aeson.TH               (Options (..), defaultOptions, deriveFromJSON)
import           Web.Internal.FormUrlEncoded as F
import           Servant.API
import           Servant.Client              (client)

import           Stripe.Types
import           Stripe.Helpers
import           Stripe.Util                 (fromJsonString)


-- TODO
-- \ * errors -- TODO better Either (esp decode fail)
-- X * pagination
--   * metadata
-- X ? request IDs
--   ? idempotency
--   ? change `String` to `Text`
--   ! flesh out data types
--   ! (define needed and) add endpoints
-- X - mv things to Stripe.Client/Stripe.Data/etc.
--   - mv things to e.g. Resource/Customer.hs, Request/Customer.hs
--   - Persistent-style TH for type definitions/JSON -- TODO TODO TODO TODO ... use Persistent to generate resource/req types+json?
--
-- TODO mv most of this to `Stripe.API` and just reexport public API via this module
--
-- TODO
--   - nested CRUD
--   - hardcode query param?
--   e.g.
--   GET https://api.stripe.com/v1/customers/{CUSTOMER_ID}/sources?object=bank_account



---- STRIPE API DATA TYPES ----

-- Resources

newtype ChargeId   = ChargeId   { unChargeId   :: T.Text } deriving (Eq, Show, Generic)
newtype CustomerId = CustomerId { unCustomerId :: T.Text } deriving (Eq, Show, Generic)

data Charge = Charge
  { chargeId       :: ChargeId
  , chargeAmount   :: Int
  , chargeCurrency :: String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 6 } ''Charge)

data Customer = Customer
  { customerId          :: CustomerId
  , customerDescription :: Maybe String
  , customerEmail       :: Maybe String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 8 } ''Customer)

-- Requests

newtype Token = Token { unToken :: String } deriving (Show, Generic)

data CustomerCreateReq = CustomerCreateReq
  { customerCreateSource      :: Token
  , customerCreateEmail       :: Maybe String
  , customerCreateDescription :: Maybe String
  } deriving (Generic)
instance F.ToForm CustomerCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
minCustomerCreateReq :: Token -> CustomerCreateReq
minCustomerCreateReq token = CustomerCreateReq token Nothing Nothing

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving (Generic)
instance F.ToForm CustomerUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
emptyCustomerUpdateReq :: CustomerUpdateReq
emptyCustomerUpdateReq = CustomerUpdateReq Nothing Nothing

instance J.FromJSON ChargeId where
  parseJSON = fromJsonString ChargeId
instance J.FromJSON CustomerId where
  parseJSON = fromJsonString CustomerId

instance ToHttpApiData ChargeId where
  toQueryParam = unChargeId
instance ToHttpApiData CustomerId where
  toQueryParam = unCustomerId
instance ToHttpApiData Token where
  toUrlPiece = T.pack . unToken


---- STRIPE API TYPE ----

type API =
  CustomerCreate :<|> CustomerRead :<|> CustomerUpdate :<|> CustomerDestroy :<|> CustomerList

type CustomerCreate  = "v1" :> "customers" :> RBody CustomerCreateReq :> StripeHeaders (PostS Customer)
type CustomerRead    = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (GetShowS Customer)
type CustomerUpdate  = "v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq :> StripeHeaders (PostS Customer)
type CustomerDestroy = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (DeleteS CustomerId)
type CustomerList    = "v1" :> "customers" :> StripePaginationQueryParams (StripeHeaders (GetListS [Customer]))



---- STRIPE ENDPOINT FUNCS ----

createCustomer :: CreateS CustomerCreateReq Customer
readCustomer :: ReadS CustomerId Customer
updateCustomer :: UpdateS CustomerId CustomerUpdateReq Customer
destroyCustomer :: DestroyS CustomerId
listCustomers :: ListS [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers = client (Proxy :: Proxy API)
