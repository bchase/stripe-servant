{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Stripe
  ( module Stripe
  , module Stripe.Types
  , module Stripe.Helpers
  ) where

import           Prelude                     hiding (ReadS)
import qualified Data.HashMap.Strict         as HM
import           Data.Char                   (toLower)
import qualified Data.Text                   as T
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Scientific             (coefficient)
import qualified Data.Time.Clock             as Time
import qualified Data.Time.Clock.POSIX       as Time
import           GHC.Generics                (Generic)

import           Data.Aeson                  as J
import           Data.Aeson.Casing           (snakeCase)
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
--   X * nested CRUD (e.g. cards, ACH)
--   X * cast `created` time ints
--   \ * metadata
--     * events (webhooks)
--     * Connect fees
--   . * ADTs for e.g. `status`
--     * expanding
--     ? `Unrecognized* jsonStr` constructor for ADTs (FromJSON)?
--     ? idempotency
--     ! flesh out data types
--     ! (define needed and) add endpoints
--   CLEANUP
--   X - mv things to Stripe.Client/Stripe.Data/etc.
--     - mv things to e.g. Resource/Customer.hs, Request/Customer.hs
--     - mv Interval/CurrencyCodeISO4217
--     - test/live key checks
--     - mv most of this to `Stripe.API` and just reexport public API via this module
--     ? change `String` to `Text`

data CurrencyCodeISO4217
  = USD
  | JPY
  | UnrecognizedCurrencyCode T.Text
  deriving (Show, Generic)
instance J.FromJSON CurrencyCodeISO4217 where
  parseJSON (J.String "usd") = return USD
  parseJSON (J.String "jpy") = return JPY
  parseJSON (J.String str)   = return . UnrecognizedCurrencyCode $ str
  parseJSON _ = mempty
instance ToHttpApiData CurrencyCodeISO4217 where
  toQueryParam = T.pack . map toLower . show
  -- toQueryParam (UnrecognizedCurrencyCode _) -- TODO ...

data Interval
  = Day
  | Week
  | Month
  | Year
  deriving (Show, Generic)
instance J.FromJSON Interval where
  parseJSON (J.String "day")   = return Day
  parseJSON (J.String "week")  = return Week
  parseJSON (J.String "month") = return Month
  parseJSON (J.String "year")  = return Year
  parseJSON _ = mempty
instance ToHttpApiData Interval where
  toQueryParam = T.pack . map toLower . show

data StripeTime = StripeTime
  { getPOSIXTime :: Int
  , getUTCTime :: Time.UTCTime
  } deriving (Eq, Show, Generic)
instance J.FromJSON StripeTime where
  parseJSON (J.Number num) =
    return $ StripeTime (fromInteger . coefficient $ num) (Time.posixSecondsToUTCTime . fromInteger . coefficient $ num)
  parseJSON _ = mempty

newtype Metadata = Metadata { unMetadata :: HM.HashMap T.Text T.Text } deriving (Show, Generic)
metadata :: [(T.Text, T.Text)] -> Metadata
metadata = Metadata . HM.fromList
instance J.FromJSON Metadata where
  parseJSON (J.Object obj) = return . Metadata . HM.map valToText $ obj
    where
      valToText v =
        case v of
          J.String txt -> txt
          _ -> "" -- TODO shouldnt ever be anything but Text
  parseJSON _ = mempty
instance ToHttpApiData Metadata where
  toQueryParam _ = "" -- TODO ... handling via `addMetadataToForm`
addMetadataToForm :: Maybe Metadata -> F.Form -> F.Form
addMetadataToForm mMetadata form =
  let hm = HM.delete "metadata" . unForm $ form
   in case mMetadata of
        Just md -> F.Form . HM.union (keysToMetadataScopedKeys . unMetadata $ md) $ hm
        Nothing -> F.Form hm
  where
    keysToMetadataScopedKeys =
      HM.fromList . map (\(k, v) -> (T.concat ["metadata[", k, "]"], [v])) . HM.toList



---- STRIPE API DATA TYPES ----

-- Resources

newtype BankAccountId = BankAccountId { unBankAccountId :: T.Text } deriving (Eq, Show, Generic)
newtype CardId        = CardId        { unCardId        :: T.Text } deriving (Eq, Show, Generic)
newtype ChargeId      = ChargeId      { unChargeId      :: T.Text } deriving (Eq, Show, Generic)
newtype CustomerId    = CustomerId    { unCustomerId    :: T.Text } deriving (Eq, Show, Generic)
newtype PlanId        = PlanId        { unPlanId        :: T.Text } deriving (Eq, Show, Generic)

newtype Token = Token { unToken :: T.Text } deriving (Show, Generic)

instance J.FromJSON BankAccountId where
  parseJSON = fromJsonString BankAccountId
instance J.FromJSON CardId where
  parseJSON = fromJsonString CardId
instance J.FromJSON ChargeId where
  parseJSON = fromJsonString ChargeId
instance J.FromJSON CustomerId where
  parseJSON = fromJsonString CustomerId
instance J.FromJSON PlanId where
  parseJSON = fromJsonString PlanId
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
instance ToHttpApiData PlanId where
  toQueryParam = unPlanId
instance ToHttpApiData Token where
  toUrlPiece = unToken

data BankAccountStatus
  = New
  | Validated
  | Verified
  | VerificationFailed
  | Errored
  deriving (Show)
instance J.FromJSON BankAccountStatus where
  parseJSON (J.String "new")                 = return New
  parseJSON (J.String "validated")           = return Validated
  parseJSON (J.String "verified")            = return Verified
  parseJSON (J.String "verification_failed") = return VerificationFailed
  parseJSON (J.String "errored")             = return Errored
  parseJSON _ = mempty

data BankAccount = BankAccount
  { bankAccountId                :: BankAccountId
  , bankAccountAccountHolderName :: String
  , bankAccountAccountHolderType :: String
  , bankAccountLast4             :: String
  , bankAccountStatus            :: BankAccountStatus
  } deriving (Show, Generic)
$(deriveFromJSON' ''BankAccount)

data Card = Card
  { cardId       :: CardId
  , cardLast4    :: String
  , cardExpMonth :: Int -- TODO
  , cardExpYear  :: Int -- TODO
  } deriving (Show, Generic)
$(deriveFromJSON' ''Card)

data Charge = Charge
  { chargeId       :: ChargeId
  , chargeAmount   :: Int
  , chargeCurrency :: String
  } deriving (Show, Generic)
$(deriveFromJSON' ''Charge)

data Customer = Customer
  { customerId          :: CustomerId
  , customerDescription :: Maybe String
  , customerEmail       :: Maybe String
  } deriving (Show, Generic)
$(deriveFromJSON' ''Customer)

data Plan = Plan
  { planId                  :: PlanId
  , planAmount              :: Int
  , planCreated             :: StripeTime
  , planCurrency            :: CurrencyCodeISO4217
  , planInterval            :: Interval
  , planIntervalCount       :: Int
  , planLivemode            :: Bool
  , planMetadata            :: Metadata
  , planName                :: String
  , planStatementDescriptor :: Maybe String
  , planTrialPeriodDays     :: Maybe Int
  } deriving (Show, Generic)
$(deriveFromJSON' ''Plan)

-- Requests

data BankAccountCreateReq = BankAccountCreateReq
  { bankAccountCreateSource :: Token
  } deriving (Generic)
instance F.ToForm BankAccountCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 17 }
minBankAccountCreateReq :: Token -> BankAccountCreateReq
minBankAccountCreateReq token = BankAccountCreateReq token

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

data PlanCreateReq = PlanCreateReq
  { planCreateId                  :: PlanId
  , planCreateName                :: String
  , planCreateAmount              :: Int
  , planCreateCurrency            :: CurrencyCodeISO4217
  , planCreateInterval            :: Interval
  , planCreateIntervalCount       :: Maybe Int
  , planCreateStatementDescriptor :: Maybe String
  , planCreateTrialPeriodDays     :: Maybe Int
  , planCreateMetadata            :: Maybe Metadata
  } deriving (Show, Generic)
instance F.ToForm PlanCreateReq where
  toForm req@PlanCreateReq{planCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
     in addMetadataToForm planCreateMetadata . toForm' $ req

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

createCustomerBankAccount :: CustomerId -> CreateS BankAccountCreateReq BankAccount
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
