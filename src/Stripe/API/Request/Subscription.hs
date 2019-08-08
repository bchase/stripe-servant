{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.API.Request.Subscription
  ( SubscriptionCreateReq (..)
  , subscriptionCreateReq

  , SubscriptionListReq (..)
  , subscriptionListReq

  , SubscriptionItemCreateReq
  , subItem
  , subItem'

  ) where

import Data.Aeson.Casing (snakeCase)
import GHC.Generics (Generic)
import Stripe.Data.Id (SubscriptionId, PlanId (unPlanId), CustomerId)
import Stripe.Types (Time, Metadata, addMetadataToForm,
                     SubscriptionStatus (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Servant.API as S
import qualified Web.Internal.FormUrlEncoded as F



data SubscriptionItemCreateReq = SubscriptionItemCreateReq
  { subscriptionItemCreatePlan     :: PlanId
  , subscriptionItemCreateQuantity :: Int -- NOTE: required, ie >0
  } deriving ( Show, Generic )

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
  -- , subscriptionListCreated       :: Maybe TimeFilter -- TODO started below
  } deriving ( Show, Generic )



---- HELPERS ----

subItem :: PlanId -> SubscriptionItemCreateReq
subItem plan = subItem' plan 1

subItem' :: PlanId -> Int -> SubscriptionItemCreateReq
subItem' = SubscriptionItemCreateReq

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
  }



---- INSTANCES ----

instance S.ToHttpApiData SubscriptionItemCreateReq where
  toQueryParam _ = "" -- TODO ... handling via `addItemsToForm`
addItemsToForm :: [SubscriptionItemCreateReq] -> F.Form -> F.Form
addItemsToForm items form = F.Form . HM.union items' $ orig
  where
    orig = HM.delete "items" . F.unForm $ form

    items' :: HM.HashMap T.Text [T.Text]
    items' = HM.fromList . concat . map conv . zip [0..] $ items

    conv :: (Int, SubscriptionItemCreateReq) -> [(T.Text, [T.Text])]
    conv (idx, SubscriptionItemCreateReq{..}) =
      let idx' = T.pack $ show idx
       in [ (T.concat ["items[", idx', "][plan]"],      [unPlanId subscriptionItemCreatePlan])
          , (T.concat ["items[", idx', "][quantity]"],  [T.pack $ show subscriptionItemCreateQuantity])
          ]


instance F.ToForm SubscriptionCreateReq where
  toForm req@SubscriptionCreateReq{subscriptionCreateItems, subscriptionCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 18 }
     in addMetadataToForm subscriptionCreateMetadata
      . addItemsToForm    subscriptionCreateItems
      $ toForm' req

instance F.ToForm SubscriptionListReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 16 }





-- TODO
-- data TimeFilter
--   = TimeFilterAt    Time
--   | TimeFilterGT    (GTFilter Time)
--   | TimeFilterLT    (LTFilter Time)
--   | TimeFilterRange (GTFilter Time) (LTFilter Time)
--   deriving ( Show, Generic )
--
-- data GTFilter a
--   = GT  a
--   | GTE a
--   deriving ( Show, Generic )
--
-- data LTFilter a
--   = LT  a
--   | LTE a
--   deriving ( Show, Generic )
--
--
-- A filter on the list based on the object created field.
-- The value can be a string with an integer Unix timestamp, or it can be a dictionary with the following options:
--   gt optional
--   Return values where the created field is after this timestamp.
--
--   gte optional
--   Return values where the created field is after or equal to this timestamp.
--
--   lt optional
--   Return values where the created field is before this timestamp.
--
--   lte optional
--   Return values where the created field is before or equal to this timestamp.
