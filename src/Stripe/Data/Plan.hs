{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Plan where

import           GHC.Generics   (Generic)

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Data.Id (PlanId)
import           Stripe.Types   (Time (..), Interval (..), Price (..),
                                 CurrencyCode (..), Metadata (..))



data Plan = Plan
  { planId                  :: PlanId
  , planAmount              :: Int
  , planCreated             :: Time
  , planCurrency            :: CurrencyCode
  , planInterval            :: Interval
  , planIntervalCount       :: Int
  , planLivemode            :: Bool
  , planMetadata            :: Metadata
  , planName                :: String
  , planStatementDescriptor :: Maybe String
  , planTrialPeriodDays     :: Maybe Int
  } deriving ( Show, Generic )



---- HELPERS ----

planPrice :: Plan -> Price
planPrice Plan{..} = Price planCurrency planAmount



---- FromJSON INSTANCES ----

$(deriveFromJSON' ''Plan)
