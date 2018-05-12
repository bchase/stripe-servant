{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.API.Request.Charge where

import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import qualified Web.Internal.FormUrlEncoded as F

import           Stripe.Data.Id (CustomerId)
import           Stripe.Types   (Metadata, StatementDescriptor,
                                 CurrencyCode, ConnectApplicationFee,
                                 Price (..), Source (..), SourceId (..),
                                 addMetadataToForm)


data ChargeCreateReq = ChargeCreateReq
  { chargeCreateAmount              :: Int
  , chargeCreateCurrency            :: CurrencyCode
  , chargeCreateCapture             :: Bool             -- NOTE: True by default          (set accordingly in `chargeCreateReq`)
  , chargeCreateCustomer            :: Maybe CustomerId -- NOTE: ONE OF THESE IS REQUIRED (enforced by `chargeCreateReq`)
  , chargeCreateSource              :: Maybe SourceId   -- NOTE: ONE OF THESE IS REQUIRED (enforced by `chargeCreateReq`)
  , chargeCreateApplicationFee      :: Maybe ConnectApplicationFee
  , chargeCreateDescription         :: Maybe String
  , chargeCreateReceiptEmail        :: Maybe String
  , chargeCreateStatementDescriptor :: Maybe StatementDescriptor
  , chargeCreateMetadata            :: Maybe Metadata
  -- , destination    -- CONNECT ONLY -- handled w/ header
  -- , transfer_group -- CONNECT ONLY
  -- , on_behalf_of   -- CONNECT ONLY
  -- , shipping       -- dictionary
  } deriving (Show, Generic)
instance F.ToForm ChargeCreateReq where
  toForm req@ChargeCreateReq{chargeCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 12 }
     in addMetadataToForm chargeCreateMetadata . toForm' $ req

chargeCreateReq :: Price -> Source -> ChargeCreateReq
chargeCreateReq price source =
  case source of
    SCustomer cust          -> (req price) { chargeCreateCustomer = Just cust }
    SCustomerCard cust card -> (req price) { chargeCreateCustomer = Just cust, chargeCreateSource = Just $ SourceCard card }
    SToken tok              -> (req price) { chargeCreateSource = Just $ SourceToken tok }
  where
    req (Price curr amount) = ChargeCreateReq
      { chargeCreateAmount              = amount
      , chargeCreateCurrency            = curr
      , chargeCreateCapture             = True
      , chargeCreateCustomer            = Nothing
      , chargeCreateSource              = Nothing
      , chargeCreateApplicationFee      = Nothing
      , chargeCreateDescription         = Nothing
      , chargeCreateReceiptEmail        = Nothing
      , chargeCreateStatementDescriptor = Nothing
      , chargeCreateMetadata            = Nothing
      }

data ChargeUpdateReq = ChargeUpdateReq
  { chargeUpdateDescription  :: Maybe String
  , chargeUpdateReceiptEmail :: Maybe String
  , chargeUpdateMetadata     :: Maybe Metadata
  -- , fraud_details  :: Maybe {...}
  -- , shipping       :: Maybe {...}
  -- , transfer_group :: Maybe ... -- Connect only
  } deriving (Show, Generic)
instance F.ToForm ChargeUpdateReq where
  toForm req@ChargeUpdateReq{chargeUpdateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 12 }
     in addMetadataToForm chargeUpdateMetadata . toForm' $ req
emptyChargeUpdateReq :: ChargeUpdateReq
emptyChargeUpdateReq = ChargeUpdateReq Nothing Nothing Nothing

data ChargeCaptureReq = ChargeCaptureReq
  { chargeCaptureAmount              :: Maybe Int
  , chargeCaptureReceiptEmail        :: Maybe String
  , chargeCaptureStatementDescriptor :: Maybe StatementDescriptor
  -- , chargeCaptureApplicationFee      :: Maybe ConnectApplicationFee
  -- , chargeCaptureDestination         :: Maybe { amount :: Int }
  } deriving (Show, Generic)
instance F.ToForm ChargeCaptureReq where
  toForm = (\n -> F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop (length . reverse . takeWhile (/= '.') . reverse . show $ n) }) ''ChargeCaptureReq -- TODO DUP1
chargeCaptureReq :: ChargeCaptureReq
chargeCaptureReq = ChargeCaptureReq Nothing Nothing Nothing
