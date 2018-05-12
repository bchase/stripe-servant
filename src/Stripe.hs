{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe
  ( module Stripe
  , module Stripe.API.HTTP
  , module Stripe.Types
  , module Stripe.Error
  , module Stripe.Helpers
  , module Stripe.Data.Id -- TODO maybe don't re-export Data.*?
  , module Stripe.Data.BankAccount
  , module Stripe.Data.Card
  , module Stripe.Data.Charge
  , module Stripe.Data.Customer
  , module Stripe.Data.Plan
  , module Stripe.API.Request.Charge
  ) where

import           Prelude                     hiding (ReadS)
import           Data.Proxy                  (Proxy (Proxy))
import           GHC.Generics                (Generic)

import           Data.Aeson.Casing           (snakeCase)
import           Web.Internal.FormUrlEncoded (toForm)
import qualified Web.Internal.FormUrlEncoded as F
import           Servant.API
import           Servant.Client              (client)

import           Stripe.Types
import           Stripe.Error
import           Stripe.Helpers
import           Stripe.Data.Id
import           Stripe.Data.BankAccount
import           Stripe.Data.Card
import           Stripe.Data.Charge
import           Stripe.Data.Customer
import           Stripe.Data.Plan
import           Stripe.API.HTTP
import           Stripe.API.Request.Charge



-- Stripe.API      -- API type & funcs
-- Stripe.API.HTTP -- req / resp / data / client -- TODO mv data, e.g. StripeScalar
-- Stripe.Error    -- `Stripe.Data.Error` instead?



---- STRIPE API DATA TYPES ----

-- Requests

data BankAccountCreate = BankAccountCreate
  { bankAccountCreateSource :: Token
  } deriving (Generic)
instance F.ToForm BankAccountCreate where
  toForm = (\n -> F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop (length . reverse . takeWhile (/= '.') . reverse . show $ n) }) ''BankAccountCreate -- TODO DUP1 TH deriveToForm
minBankAccountCreateReq :: Token -> BankAccountCreate
minBankAccountCreateReq token = BankAccountCreate token

data BankAccountUpdateReq = BankAccountUpdateReq
  { bankAccountUpdateAccountHolderName :: Maybe String
  , bankAccountUpdateAccountHolderType :: Maybe String
  } deriving (Generic)
instance F.ToForm BankAccountUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 17 }
emptyBankAccountUpdateReq :: BankAccountUpdateReq
emptyBankAccountUpdateReq = BankAccountUpdateReq Nothing Nothing

data BankAccountVerifyReq = BankAccountVerifyReq
  { bankAccountVerifyAmount1 :: Int
  , bankAccountVerifyAmount2 :: Int
  } deriving (Generic)
instance F.ToForm BankAccountVerifyReq where
  toForm req =
    [ ("amounts[]", toQueryParam . bankAccountVerifyAmount1 $ req)
    , ("amounts[]", toQueryParam . bankAccountVerifyAmount2 $ req)
    ]

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
customerCreateReq :: Token -> CustomerCreateReq
customerCreateReq token = CustomerCreateReq token Nothing Nothing

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving (Generic)
instance F.ToForm CustomerUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
emptyCustomerUpdateReq :: CustomerUpdateReq
emptyCustomerUpdateReq = CustomerUpdateReq Nothing Nothing

data PlanCreateReq = PlanCreateReq
  { planCreateId                  :: PlanId
  , planCreateName                :: String
  , planCreateAmount              :: Int
  , planCreateCurrency            :: CurrencyCode
  , planCreateInterval            :: Interval
  , planCreateIntervalCount       :: Maybe Int
  , planCreateStatementDescriptor :: Maybe StatementDescriptor
  , planCreateTrialPeriodDays     :: Maybe Int
  , planCreateMetadata            :: Maybe Metadata
  } deriving (Show, Generic)
instance F.ToForm PlanCreateReq where
  toForm req@PlanCreateReq{planCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
     in addMetadataToForm planCreateMetadata . toForm' $ req

planCreateReq :: PlanId -> String -> Price -> Interval -> PlanCreateReq
planCreateReq pid name (Price curr amount) intv = PlanCreateReq
  { planCreateId                  = pid
  , planCreateName                = name
  , planCreateAmount              = amount
  , planCreateCurrency            = curr
  , planCreateInterval            = intv
  , planCreateIntervalCount       = Nothing
  , planCreateStatementDescriptor = Nothing
  , planCreateTrialPeriodDays     = Nothing
  , planCreateMetadata            = Nothing
  }

data PlanUpdateReq = PlanUpdateReq
  { planUpdateName                :: Maybe String
  , planUpdateStatementDescriptor :: Maybe String
  , planUpdateMetadata            :: Maybe Metadata
  } deriving (Show, Generic)
instance F.ToForm PlanUpdateReq where
  toForm req@PlanUpdateReq{planUpdateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
     in addMetadataToForm planUpdateMetadata . toForm' $ req
emptyPlanUpdateReq :: PlanUpdateReq
emptyPlanUpdateReq = PlanUpdateReq Nothing Nothing Nothing



---- STRIPE API TYPE ----

type CustomerAPI = CustomerCreate :<|> CustomerRead :<|> CustomerUpdate :<|> CustomerDestroy :<|> CustomerList
type CustomerCardAPI = CustomerCardCreate :<|> CustomerCardRead :<|> CustomerCardUpdate :<|> CustomerCardDestroy :<|> CustomerCardList
type CustomerBankAccountAPI = CustomerBankAccountCreate :<|> CustomerBankAccountRead :<|> CustomerBankAccountUpdate :<|> CustomerBankAccountDestroy :<|> CustomerBankAccountList :<|> CustomerBankAccountVerify
type PlanAPI = PlanCreate :<|> PlanRead :<|> PlanUpdate :<|> PlanDestroy :<|> PlanList

type ChargeAPI = ChargeCreate :<|> ChargeRead :<|> ChargeUpdate :<|> ChargeList :<|> ChargeCapture


type ChargeCreate  = "v1" :> "charges" :> RBody ChargeCreateReq :> StripeHeaders (PostS Charge)
type ChargeRead    = "v1" :> "charges" :> CapId ChargeId :> StripeHeaders (GetShowS Charge)
type ChargeUpdate  = "v1" :> "charges" :> CapId ChargeId :> RBody ChargeUpdateReq :> StripeHeaders (PostS Charge)
type ChargeList    = "v1" :> "charges" :> StripePaginationQueryParams (StripeHeaders (GetListS [Charge]))
type ChargeCapture = "v1" :> "charges" :> CapId ChargeId :> "capture" :> RBody ChargeCaptureReq :> StripeHeaders (PostS Charge)

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

type CustomerBankAccountCreate  = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> RBody BankAccountCreate :> StripeHeaders (PostS BankAccount)
type CustomerBankAccountRead    = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> StripeHeaders (GetShowS BankAccount)
type CustomerBankAccountUpdate  = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> RBody BankAccountUpdateReq :> StripeHeaders (PostS BankAccount)
type CustomerBankAccountDestroy = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> StripeHeaders (DeleteS BankAccountId)
type CustomerBankAccountList    = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> StripePaginationQueryParams (StripeHeaders (GetListS [BankAccount]))
type CustomerBankAccountVerify  = "v1" :> "customers" :> CapId CustomerId :> "bank_accounts" :> CapId BankAccountId :> "verify" :> RBody BankAccountVerifyReq :> StripeHeaders (PostS BankAccount)

type PlanCreate  = "v1" :> "plans" :> RBody PlanCreateReq :> StripeHeaders (PostS Plan)
type PlanRead    = "v1" :> "plans" :> CapId PlanId :> StripeHeaders (GetShowS Plan)
type PlanUpdate  = "v1" :> "plans" :> CapId PlanId :> RBody PlanUpdateReq :> StripeHeaders (PostS Plan)
type PlanDestroy = "v1" :> "plans" :> CapId PlanId :> StripeHeaders (DeleteS PlanId)
type PlanList    = "v1" :> "plans" :> StripePaginationQueryParams (StripeHeaders (GetListS [Plan]))


---- STRIPE ENDPOINT FUNCS ----

createCustomer :: CreateS CustomerCreateReq Customer
readCustomer :: ReadS CustomerId Customer
updateCustomer :: UpdateS CustomerId CustomerUpdateReq Customer
destroyCustomer :: DestroyS CustomerId
listCustomers :: ListS [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers =
  client (Proxy :: Proxy CustomerAPI)

createCustomerCard :: CustomerId -> CreateS CardCreateReq Card
readCustomerCard :: CustomerId -> ReadS CardId Card
updateCustomerCard :: CustomerId -> UpdateS CardId CardUpdateReq Card
destroyCustomerCard :: CustomerId -> DestroyS CardId
listCustomerCards :: CustomerId -> ListS [Card]
createCustomerCard :<|> readCustomerCard :<|> updateCustomerCard :<|> destroyCustomerCard :<|> listCustomerCards =
  client (Proxy :: Proxy CustomerCardAPI)

createCustomerBankAccount :: CustomerId -> CreateS BankAccountCreate BankAccount
readCustomerBankAccount :: CustomerId -> ReadS BankAccountId BankAccount
updateCustomerBankAccount :: CustomerId -> UpdateS BankAccountId BankAccountUpdateReq BankAccount
destroyCustomerBankAccount :: CustomerId -> DestroyS BankAccountId
listCustomerBankAccounts :: CustomerId -> ListS [BankAccount]
verifyCustomerBankAccount :: CustomerId -> UpdateS BankAccountId BankAccountVerifyReq BankAccount
createCustomerBankAccount :<|> readCustomerBankAccount :<|> updateCustomerBankAccount :<|> destroyCustomerBankAccount :<|> listCustomerBankAccounts :<|> verifyCustomerBankAccount =
  client (Proxy :: Proxy CustomerBankAccountAPI)

createPlan :: CreateS PlanCreateReq Plan
readPlan :: ReadS PlanId Plan
updatePlan :: UpdateS PlanId PlanUpdateReq Plan
destroyPlan :: DestroyS PlanId
listPlans :: ListS [Plan]
createPlan :<|> readPlan :<|> updatePlan :<|> destroyPlan :<|> listPlans =
  client (Proxy :: Proxy PlanAPI)

createCharge :: CreateS ChargeCreateReq Charge
readCharge :: ReadS ChargeId Charge
updateCharge :: UpdateS ChargeId ChargeUpdateReq Charge
listCharges :: ListS [Charge]
captureCharge :: ChargeId -> CreateS ChargeCaptureReq Charge
createCharge :<|> readCharge :<|> updateCharge :<|> listCharges :<|> captureCharge =
  client (Proxy :: Proxy ChargeAPI)



-- -- CURRENTLY FOR TESTING ONLY -- --

-- Create Bank Account Token --

data BankAccountToken = BankAccountToken
  { bankAccountTokenId :: Token
  } deriving (Show, Generic)
$(deriveFromJSON' ''BankAccountToken)

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

type BankAccountTokenCreate = "v1" :> "tokens" :> RBody BankAccountTokenCreateReq :> StripeHeaders (PostS BankAccountToken)

createBankAccountToken :: CreateS BankAccountTokenCreateReq BankAccountToken
createBankAccountToken = client (Proxy :: Proxy BankAccountTokenCreate)
