{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.API.Request.Subscription
  ( SubscriptionCreateReq (..)
  , subscriptionCreateReq

  , SubscriptionListReq (..)
  , subscriptionListReq

  ) where

import Data.Aeson.Casing (snakeCase)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Web.Internal.FormUrlEncoded as F

import Stripe.API.Request.SubscriptionItem (SubscriptionItemCreateReq (..))
import Stripe.Data.Id (SubscriptionId, PlanId (unPlanId), CustomerId)
import Stripe.Types (Time, Metadata, addMetadataToForm,
                     SubscriptionStatus (..),
                     TimeFilter, timeFilterToFormFields)
import Stripe.Util (addToForm)



data SubscriptionCreateReq = SubscriptionCreateReq
  { subscriptionCreateCustomer               :: CustomerId
  , subscriptionCreateItems                  :: [SubscriptionItemCreateReq] -- NOTE: required (non-empty)
  , subscriptionCreateApplicationFreePercent :: Maybe Float                 -- NOTE: 0-100%, to 2 decimal places
  , subscriptionCreateTaxPercent             :: Maybe Float                 -- NOTE: 0-100%, to 4 decimal places
  , subscriptionCreateTrialEnd               :: Maybe Time
  , subscriptionCreateTrialPeriodDays        :: Maybe Int
  , subscriptionCreateCoupon                 :: Maybe String
  , subscriptionCreateMetadata               :: Maybe Metadata
  } deriving ( Show, Generic )

data SubscriptionListReq = SubscriptionListReq
  { subscriptionListLimit         :: Maybe Int -- [1..100] default 10
  , subscriptionListEndingBefore  :: Maybe SubscriptionId
  , subscriptionListStartingAfter :: Maybe SubscriptionId
  , subscriptionListStatus        :: Maybe SubscriptionStatus
  , subscriptionListPlan          :: Maybe PlanId
  , subscriptionListCustomer      :: Maybe CustomerId
  , subscriptionListCreated       :: Maybe TimeFilter -- TODO `TimeFilter` mostly untested
  } deriving ( Show, Generic )



---- HELPERS ----

subscriptionCreateReq :: CustomerId -> [SubscriptionItemCreateReq] -> SubscriptionCreateReq
subscriptionCreateReq cust items = SubscriptionCreateReq
  { subscriptionCreateCustomer               = cust
  , subscriptionCreateItems                  = items
  , subscriptionCreateApplicationFreePercent = Nothing
  , subscriptionCreateTaxPercent             = Nothing
  , subscriptionCreateTrialEnd               = Nothing
  , subscriptionCreateTrialPeriodDays        = Nothing
  , subscriptionCreateCoupon                 = Nothing
  , subscriptionCreateMetadata               = Nothing
  }

subscriptionListReq :: SubscriptionListReq
subscriptionListReq = SubscriptionListReq
  { subscriptionListLimit         = Nothing
  , subscriptionListEndingBefore  = Nothing
  , subscriptionListStartingAfter = Nothing
  , subscriptionListStatus        = Nothing
  , subscriptionListPlan          = Nothing
  , subscriptionListCustomer      = Nothing
  , subscriptionListCreated       = Nothing
  }



---- INSTANCES ----

instance F.ToForm SubscriptionCreateReq where
  toForm req@SubscriptionCreateReq{subscriptionCreateItems, subscriptionCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 18 }
     in addMetadataToForm subscriptionCreateMetadata
      . addItemsToForm    subscriptionCreateItems
      $ toForm' req

instance F.ToForm SubscriptionListReq where
  toForm req@SubscriptionListReq{subscriptionListCreated} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 16 }
        key = "created"
        fs  = fromMaybe [] $ timeFilterToFormFields key <$> subscriptionListCreated
     in addToForm key fs
      $ toForm' req

addItemsToForm :: [SubscriptionItemCreateReq] -> F.Form -> F.Form
addItemsToForm items form = addToForm "items" (subItemsToFormFields items) form
  where
    subItemsToFormFields = foldr f [] . zip ([0..] :: [Int])
    f (idx, SubscriptionItemCreateReq{subscriptionItemCreatePlan, subscriptionItemCreateQuantity}) acc =
       ( (T.concat ["items[", T.pack $ show idx, "][plan]"],     [unPlanId      subscriptionItemCreatePlan    ])
       : (T.concat ["items[", T.pack $ show idx, "][quantity]"], [T.pack $ show subscriptionItemCreateQuantity])
       : acc
       )
