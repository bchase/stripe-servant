{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Stripe.Unsafe.BankAccountToken where

import           Data.Proxy   (Proxy (Proxy))
import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import           Web.Internal.FormUrlEncoded (toForm)
import qualified Web.Internal.FormUrlEncoded as F
import           Servant.API                 ((:>))
import           Servant.Client              (client)

import           Stripe.API.HTTP (StripeHeaders, RBody, CreateS, PostS)
import           Stripe.Data.Id  (Token)
import           Stripe.Helpers  (deriveFromJSON')



---- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----
---- !!! "Unsafe" because bank account details hit the server !!! ----
---- !!!           (provided for testing purposes)            !!! ----
---- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----

type BankAccountTokenCreate = "v1" :> "tokens" :> RBody BankAccountTokenCreateReq :> StripeHeaders (PostS BankAccountToken)

createBankAccountToken :: CreateS BankAccountTokenCreateReq BankAccountToken
createBankAccountToken = client (Proxy :: Proxy BankAccountTokenCreate)



---- REQUESTS ----

data BankAccountToken = BankAccountToken -- TODO newtype
  { bankAccountTokenId :: Token
  } deriving ( Show, Generic )

data BankAccountTokenCreateReq = BankAccountTokenCreateReq
  { bankAccountTokenCreateCountry           :: String
  , bankAccountTokenCreateCurrency          :: String
  , bankAccountTokenCreateAccountHolderName :: String
  , bankAccountTokenCreateAccountHolderType :: String
  , bankAccountTokenCreateRoutingNumber     :: String
  , bankAccountTokenCreateAccountNumber     :: String
  } deriving ( Generic )



---- ToForm & FromJSON INSTANCES ----

$(deriveFromJSON' ''BankAccountToken)

instance F.ToForm BankAccountTokenCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = (\v -> "bank_account[" ++ v ++ "]") . snakeCase . drop 22 }
