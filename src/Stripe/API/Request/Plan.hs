{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripe.API.Request.Plan where

import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import qualified Web.Internal.FormUrlEncoded as F

import           Stripe.Data.Id (PlanId)
import           Stripe.Types   (Metadata, Interval, StatementDescriptor,
                                 CurrencyCode, Price (..),
                                 Metadata, addMetadataToForm)



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
  } deriving ( Show, Generic )

data PlanUpdateReq = PlanUpdateReq
  { planUpdateName                :: Maybe String
  , planUpdateStatementDescriptor :: Maybe String
  , planUpdateMetadata            :: Maybe Metadata
  } deriving ( Show, Generic )



---- HELPERS ----

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

planUpdateReq :: PlanUpdateReq
planUpdateReq = PlanUpdateReq Nothing Nothing Nothing



---- ToForm INSTANCES ----

instance F.ToForm PlanCreateReq where
  toForm req@PlanCreateReq{planCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
     in addMetadataToForm planCreateMetadata . toForm' $ req

instance F.ToForm PlanUpdateReq where
  toForm req@PlanUpdateReq{planUpdateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
     in addMetadataToForm planUpdateMetadata . toForm' $ req
