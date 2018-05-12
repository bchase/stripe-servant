{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Card where

import           GHC.Generics   (Generic)

import           Data.Aeson     as J

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Data.Id (CardId, CustomerId)
import           Stripe.Types   (CountryCode (..), Metadata (..))



data Card = Card
  { cardId                 :: CardId
  , cardAddressCity        :: Maybe String
  , cardAddressCountry     :: Maybe String
  , cardAddressLine1       :: Maybe String
  , cardAddressLine1Check  :: Maybe Check
  , cardAddressLine2       :: Maybe String
  , cardAddressState       :: Maybe String
  , cardAddressZip         :: Maybe String
  , cardAddressZipCheck    :: Maybe Check
  , cardBrand              :: CardBrand
  , cardCountry            :: CountryCode
  , cardCustomer           :: CustomerId
  , cardCvcCheck           :: Maybe Check
  , cardDynamicLast4       :: Maybe String
  , cardExpMonth           :: Int
  , cardExpYear            :: Int
  , cardFingerprint        :: String
  , cardFunding            :: CardFundingType
  , cardLast4              :: String
  , cardMetadata           :: Metadata
  , cardName               :: Maybe String
  , cardTokenizationMethod :: Maybe TokenizationMethod
  } deriving (Show, Generic)



---- RELATED TYPES ----

data CardBrand
  = AmericanExpress
  | DinersClub
  | Discover
  | JCB
  | MasterCard
  | Visa
  | UnknownCardBrand
  deriving ( Show, Generic )

data CardFundingType
  = Credit
  | Debit
  | Prepaid
  | UnknownCardFundingType
  deriving ( Show, Generic )

data TokenizationMethod
  = ApplePay
  | AndroidPay
  deriving ( Show, Generic )

data Check
  = Pass
  | Fail
  | Unavailable
  | Unchecked
  deriving ( Show, Generic )



---- FromJSON INSTANCES ----

$(deriveFromJSON' ''Card)

instance J.FromJSON CardBrand where
  parseJSON (J.String "Visa")             = return Visa
  parseJSON (J.String "American Express") = return AmericanExpress
  parseJSON (J.String "MasterCard")       = return MasterCard
  parseJSON (J.String "Discover")         = return Discover
  parseJSON (J.String "JCB")              = return JCB
  parseJSON (J.String "Diners Club")      = return DinersClub
  parseJSON (J.String "Unknown")          = return UnknownCardBrand
  parseJSON _ = mempty

instance J.FromJSON CardFundingType where
  parseJSON (J.String "credit")  = return Credit
  parseJSON (J.String "debit")   = return Debit
  parseJSON (J.String "prepaid") = return Prepaid
  parseJSON (J.String "unknown") = return UnknownCardFundingType
  parseJSON _ = mempty

instance J.FromJSON TokenizationMethod where
  parseJSON (J.String "apply_pay")   = return ApplePay
  parseJSON (J.String "android_pay") = return AndroidPay
  parseJSON _ = mempty

instance J.FromJSON Check where
  parseJSON (J.String "pass")        = return Pass
  parseJSON (J.String "fail")        = return Fail
  parseJSON (J.String "unavailable") = return Unavailable
  parseJSON (J.String "unchecked")   = return Unchecked
  parseJSON _ = mempty
