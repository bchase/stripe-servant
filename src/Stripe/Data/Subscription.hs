{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Subscription where

import           GHC.Generics (Generic)

import           Stripe.Util      (deriveFromJSON')
import           Stripe.Data.Id   (SubscriptionId, SubscriptionItemId, CustomerId)
import           Stripe.Data.Plan (Plan)
import           Stripe.Types     (Time, Metadata,
                                   SubscriptionStatus, Discount)



data Subscription = Subscription
  { subscriptionId                     :: SubscriptionId
  , subscriptionStatus                 :: SubscriptionStatus
  , subscriptionCustomer               :: CustomerId
  , subscriptionPlan                   :: Plan
  , subscriptionQuantity               :: Int
  , subscriptionItems                  :: ResourceList SubscriptionItem

  , subscriptionApplicationFreePercent :: Maybe Float -- NOTE: 0-100%, to 2 decimal places
  , subscriptionTaxPercent             :: Maybe Float -- NOTE: 0-100%, to 4 decimal places
  , subscriptionDiscount               :: Maybe Discount

  , subscriptionCurrentPeriodStart     :: Maybe Time
  , subscriptionCurrentPeriodEnd       :: Maybe Time
  , subscriptionTrialStart             :: Maybe Time
  , subscriptionTrialEnd               :: Maybe Time
  , subscriptionCreated                :: Maybe Time
  , subscriptionEndedAt                :: Maybe Time
  , subscriptionCanceledAt             :: Maybe Time
  , subscriptionCancelAtPeriodEnd      :: Bool

  , subscriptionMetadata               :: Metadata
  } deriving ( Show, Generic )



data ResourceList a = ResourceList
  { resourceListData    :: [a]
  , resourceListHasMore :: Bool
  , resourceListUrl     :: String
  } deriving ( Show, Generic )

data SubscriptionItem = SubscriptionItem
  { subscriptionItemId       :: SubscriptionItemId
  , subscriptionItemCreated  :: Time
  , subscriptionItemQuantity :: Int -- NOTE: non-negative
  , subscriptionItemPlan     :: Plan
  , subscriptionItemMetadata :: Metadata
  } deriving ( Show, Generic )



---- HELPERS ----

---- FromJSON INSTANCES ----

$(deriveFromJSON' ''ResourceList)
$(deriveFromJSON' ''SubscriptionItem)
$(deriveFromJSON' ''Subscription)
