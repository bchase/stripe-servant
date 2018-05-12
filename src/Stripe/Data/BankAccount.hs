{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.BankAccount where

import           GHC.Generics   (Generic)

import           Data.Aeson     as J

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Data.Id (BankAccountId, AccountId)
import           Stripe.Types   (CurrencyCode (..), CountryCode (..),
                                 Metadata (..))



data BankAccount = BankAccount
  { bankAccountId                 :: BankAccountId
  , bankAccountAccount            :: Maybe AccountId
  , bankAccountAccountHolderName  :: String
  , bankAccountAccountHolderType  :: BankAccountHolderType
  , bankAccountBankName           :: String
  , bankAccountCountry            :: CountryCode
  , bankAccountCurrency           :: CurrencyCode
  , bankAccountDefaultForCurrency :: Maybe Bool
  , bankAccountFingerprint        :: String
  , bankAccountLast4              :: String
  , bankAccountMetadata           :: Metadata
  , bankAccountRoutingNumber      :: String
  , bankAccountStatus             :: BankAccountStatus
  } deriving (Show, Generic)



---- RELATED TYPES ----

data BankAccountStatus
  = New
  | Validated
  | Verified
  | VerificationFailed
  | Errored
  deriving ( Show )

data BankAccountHolderType
  = Individual
  | Company
  deriving ( Show, Generic )



---- FromJSON INSTANCES ----

$(deriveFromJSON' ''BankAccount)

instance J.FromJSON BankAccountStatus where
  parseJSON (J.String "new")                 = return New
  parseJSON (J.String "validated")           = return Validated
  parseJSON (J.String "verified")            = return Verified
  parseJSON (J.String "verification_failed") = return VerificationFailed
  parseJSON (J.String "errored")             = return Errored
  parseJSON _ = mempty

instance J.FromJSON BankAccountHolderType where
  parseJSON (J.String "individual") = return Individual
  parseJSON (J.String "company")    = return Company
  parseJSON _ = mempty
