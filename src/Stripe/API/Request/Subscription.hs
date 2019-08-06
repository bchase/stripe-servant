{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.API.Request.Subscription
  ( SubscriptionCreateReq (..)
  , subscriptionCreateReq

  , SubscriptionItemCreateReq
  , subItem
  , subItem'
  ) where

import Data.Aeson.Casing (snakeCase)
import GHC.Generics (Generic)
import Stripe.Data.Id (PlanId (unPlanId), CustomerId)
import Stripe.Types (Time, Metadata, addMetadataToForm)

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



---- INSTANCES ----

instance F.ToForm SubscriptionCreateReq where
  toForm req@SubscriptionCreateReq{subscriptionCreateItems, subscriptionCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 18 }
     in addMetadataToForm subscriptionCreateMetadata
      . addItemsToForm    subscriptionCreateItems
      $ toForm' req

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
