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
--   GENERAL STRIPE
--   \ * errors -- TODO better Either (esp decode fail)
--   X * pagination
--   X * request IDs
--     * metadata
--     * cast `created` time ints
--     * expanding
--     * nested CRUD (e.g. cards, ACH)
--     ? idempotency
--     ! flesh out data types
--     ! (define needed and) add endpoints
--   CLEANUP
--   X - mv things to Stripe.Client/Stripe.Data/etc.
--     - mv things to e.g. Resource/Customer.hs, Request/Customer.hs
--     - test/live key checks
--     - mv most of this to `Stripe.API` and just reexport public API via this module
--     ? change `String` to `Text`



---- STRIPE API DATA TYPES ----

-- Resources

newtype BankAccountId = BankAccountId { unBankAccountId :: T.Text } deriving (Eq, Show, Generic)
newtype CardId        = CardId        { unCardId        :: T.Text } deriving (Eq, Show, Generic)
newtype ChargeId      = ChargeId      { unChargeId      :: T.Text } deriving (Eq, Show, Generic)
newtype CustomerId    = CustomerId    { unCustomerId    :: T.Text } deriving (Eq, Show, Generic)

newtype Token = Token { unToken :: T.Text } deriving (Show, Generic)

instance J.FromJSON BankAccountId where
  parseJSON = fromJsonString BankAccountId
instance J.FromJSON CardId where
  parseJSON = fromJsonString CardId
instance J.FromJSON ChargeId where
  parseJSON = fromJsonString ChargeId
instance J.FromJSON CustomerId where
  parseJSON = fromJsonString CustomerId
instance J.FromJSON Token where
  parseJSON = fromJsonString Token

instance ToHttpApiData BankAccountId where
  toQueryParam = unBankAccountId
instance ToHttpApiData CardId where
  toQueryParam = unCardId
instance ToHttpApiData ChargeId where
  toQueryParam = unChargeId
instance ToHttpApiData CustomerId where
  toQueryParam = unCustomerId
instance ToHttpApiData Token where
  toUrlPiece = unToken


data BankAccount = BankAccount
  { bankAccountId    :: BankAccountId
  , bankAccountLast4 :: String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 11 } ''BankAccount)

data Card = Card
  { cardId       :: CardId
  , cardLast4    :: String
  , cardExpMonth :: Int -- TODO
  , cardExpYear  :: Int -- TODO
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 4 } ''Card)

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

data BankAccountCreateReq = BankAccountCreateReq
  { bankAccountCreateSource :: Token
  } deriving (Generic)
instance F.ToForm BankAccountCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 17 }
minBankAccountCreateReq :: Token -> BankAccountCreateReq
minBankAccountCreateReq token = BankAccountCreateReq token

-- data BankAccountUpdateReq = BankAccountUpdateReq
--   { bankAccountUpdateExpMonth :: Maybe Int
--   , bankAccountUpdateExpYear  :: Maybe Int
--   } deriving (Generic)
-- instance F.ToForm BankAccountUpdateReq where
--   toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
-- emptyBankAccountUpdateReq :: BankAccountUpdateReq
-- emptyBankAccountUpdateReq = BankAccountUpdateReq Nothing Nothing

data CardCreateReq = CardCreateReq
  { cardCreateSource :: Token
  } deriving (Generic)
instance F.ToForm CardCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
minCardCreateReq :: Token -> CardCreateReq
minCardCreateReq token = CardCreateReq token

data CardUpdateReq = CardUpdateReq
  { cardUpdateExpMonth :: Maybe Int
  , cardUpdateExpYear  :: Maybe Int
  } deriving (Generic)
instance F.ToForm CardUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
emptyCardUpdateReq :: CardUpdateReq
emptyCardUpdateReq = CardUpdateReq Nothing Nothing

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


---- STRIPE API TYPE ----

type CustomerAPI = CustomerCreate     :<|> CustomerRead     :<|> CustomerUpdate     :<|> CustomerDestroy     :<|> CustomerList
type CustomerCardAPI = CustomerCardCreate :<|> CustomerCardRead :<|> CustomerCardUpdate :<|> CustomerCardDestroy :<|> CustomerCardList
type CustomerBankAccountAPI =
  CustomerBankAccountCreate :<|>
  -- CustomerBankAccountRead :<|>
  -- CustomerBankAccountUpdate :<|>
  -- CustomerBankAccountDestroy :<|>
  CustomerBankAccountList


type CustomerCreate  = "v1" :> "customers" :> RBody CustomerCreateReq :> StripeHeaders (PostS Customer)
type CustomerRead    = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (GetShowS Customer)
type CustomerUpdate  = "v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq :> StripeHeaders (PostS Customer)
type CustomerDestroy = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (DeleteS CustomerId)
type CustomerList    = "v1" :> "customers" :> StripePaginationQueryParams (StripeHeaders (GetListS [Customer]))

type CustomerCardCreate  = "v1" :> "customers" :> CapId CustomerId :> "cards" :> RBody CardCreateReq :> StripeHeaders (PostS Card)
type CustomerCardRead    = "v1" :> "customers" :> CapId CustomerId :> "cards" :> CapId CardId :> StripeHeaders (GetShowS Card)
type CustomerCardUpdate  = "v1" :> "customers" :> CapId CustomerId :> "cards" :> CapId CardId :> RBody CardUpdateReq :> StripeHeaders (PostS Card)
type CustomerCardDestroy = "v1" :> "customers" :> CapId CustomerId :> "cards" :> CapId CardId :> StripeHeaders (DeleteS CardId)
type CustomerCardList    = "v1" :> "customers" :> CapId CustomerId :> "cards" :> StripePaginationQueryParams (StripeHeaders (GetListS [Card]))

type CustomerBankAccountCreate  = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> RBody BankAccountCreateReq :> StripeHeaders (PostS BankAccount)
-- type CustomerBankAccountRead    = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> StripeHeaders (GetShowS BankAccount)
-- type CustomerBankAccountUpdate  = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> RBody BankAccountUpdateReq :> StripeHeaders (PostS BankAccount)
-- type CustomerBankAccountDestroy = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> StripeHeaders (DeleteS BankAccountId)
type CustomerBankAccountList    = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> StripePaginationQueryParams (StripeHeaders (GetListS [BankAccount]))



---- STRIPE ENDPOINT FUNCS ----

createCustomer :: CreateS CustomerCreateReq Customer
readCustomer :: ReadS CustomerId Customer
updateCustomer :: UpdateS CustomerId CustomerUpdateReq Customer
destroyCustomer :: DestroyS CustomerId
listCustomers :: ListS [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers = client (Proxy :: Proxy CustomerAPI)

createCustomerCard :: CustomerId -> CreateS CardCreateReq Card
readCustomerCard :: CustomerId -> ReadS CardId Card
updateCustomerCard :: CustomerId -> UpdateS CardId CardUpdateReq Card
destroyCustomerCard :: CustomerId -> DestroyS CardId
listCustomerCards :: CustomerId -> ListS [Card]
createCustomerCard :<|> readCustomerCard :<|> updateCustomerCard :<|> destroyCustomerCard :<|> listCustomerCards = client (Proxy :: Proxy CustomerCardAPI)

createCustomerBankAccount :: CustomerId -> CreateS BankAccountCreateReq BankAccount
-- readCustomerBankAccount :: CustomerId -> ReadS BankAccountId BankAccount
-- updateCustomerBankAccount :: CustomerId -> UpdateS BankAccountId BankAccountUpdateReq BankAccount
-- destroyCustomerBankAccount :: CustomerId -> DestroyS BankAccountId
listCustomerBankAccounts :: CustomerId -> ListS [BankAccount]
createCustomerBankAccount :<|> listCustomerBankAccounts = client (Proxy :: Proxy CustomerBankAccountAPI)



-- -- CURRENTLY FOR TESTING ONLY -- --

-- Create Bank Account Token --

data BankAccountToken = BankAccountToken
  { bankAccountTokenId :: Token
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 16 } ''BankAccountToken)

data BankAccountTokenCreateReq = BankAccountTokenCreateReq
  { bankAccountTokenCreateCountry           :: String
  , bankAccountTokenCreateCurrency          :: String
  , bankAccountTokenCreateAccountHolderName :: String
  , bankAccountTokenCreateAccountHolderType :: String
  , bankAccountTokenCreateRoutingNumber     :: String
  , bankAccountTokenCreateAccountNumber     :: String
  } deriving (Generic)
instance F.ToForm BankAccountTokenCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = (\v -> "bank_account[" ++ v ++ "]") . snakeCase . drop 22 }
testBankAccountTokenCreateReq :: BankAccountTokenCreateReq
testBankAccountTokenCreateReq =
  BankAccountTokenCreateReq
    { bankAccountTokenCreateCountry           = "US"
    , bankAccountTokenCreateCurrency          = "usd"
    , bankAccountTokenCreateAccountHolderName = "Olivia Harris"
    , bankAccountTokenCreateAccountHolderType = "individual"
    , bankAccountTokenCreateRoutingNumber     = "110000000"
    , bankAccountTokenCreateAccountNumber     = "000123456789"
    }

type BankAccountTokenCreate = "v1" :> "tokens" :> RBody BankAccountTokenCreateReq :> StripeHeaders (PostS BankAccountToken)

createBankAccountToken :: CreateS BankAccountTokenCreateReq BankAccountToken
createBankAccountToken = client (Proxy :: Proxy BankAccountTokenCreate)

getTestBankAccountToken :: StripeSecretKey -> IO Token
getTestBankAccountToken key = do
  resp <- stripeScalar' key WithoutConnect $ createBankAccountToken testBankAccountTokenCreateReq
  case resp of
    Left  err -> putStrLn "[[FAIL Stripe.getTestBankAccountToken]]" >> print err >> error ""
    Right bat -> return . bankAccountTokenId . stripeScalarData $ bat
