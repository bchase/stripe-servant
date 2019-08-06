{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.API.Request.Subscription
  ( SubscriptionCreateReq (..)
  , subscriptionCreateReq

  , SubscriptionItem
  , subItem
  , subItem'
  ) where

import Data.Aeson.Casing (snakeCase)
import GHC.Generics (Generic)
import Stripe.Data.Id (PlanId (unPlanId), CustomerId)
import Stripe.Types (Metadata, addMetadataToForm)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Servant.API as S
import qualified Web.Internal.FormUrlEncoded as F



data SubscriptionItem = SubscriptionItem
  { subscriptionItemPlan     :: PlanId
  , subscriptionItemQuantity :: Int -- NOTE: required, ie >0
  } deriving ( Show, Generic )

data SubscriptionCreateReq = SubscriptionCreateReq
  { subscriptionCreateCustomer :: CustomerId
  , subscriptionCreateItems    :: [SubscriptionItem] -- NOTE: required (non-empty)
  , subscriptionCreateMetadata :: Maybe Metadata
  } deriving ( Show, Generic )



---- HELPERS ----

subItem :: PlanId -> SubscriptionItem
subItem plan = subItem' plan 1

subItem' :: PlanId -> Int -> SubscriptionItem
subItem' = SubscriptionItem

subscriptionCreateReq :: CustomerId -> [SubscriptionItem] -> SubscriptionCreateReq
subscriptionCreateReq cust items = SubscriptionCreateReq
  { subscriptionCreateCustomer = cust
  , subscriptionCreateItems    = items
  , subscriptionCreateMetadata = Nothing
  }



---- INSTANCES ----

instance F.ToForm SubscriptionCreateReq where
  toForm req@SubscriptionCreateReq{subscriptionCreateItems, subscriptionCreateMetadata} =
    let toForm' = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 18 }
     in addMetadataToForm subscriptionCreateMetadata
      . addItemsToForm    subscriptionCreateItems
      $ toForm' req

instance S.ToHttpApiData SubscriptionItem where
  toQueryParam _ = "" -- TODO ... handling via `addItemsToForm`
addItemsToForm :: [SubscriptionItem] -> F.Form -> F.Form
addItemsToForm items form = F.Form . HM.union items' $ orig
  where
    orig = HM.delete "items" . F.unForm $ form

    items' :: HM.HashMap T.Text [T.Text]
    items' = HM.fromList . concat . map conv . zip [0..] $ items

    conv :: (Int, SubscriptionItem) -> [(T.Text, [T.Text])]
    conv (idx, SubscriptionItem{..}) =
      let idx' = T.pack $ show idx
       in [ (T.concat ["items[", idx', "][plan]"],      [unPlanId subscriptionItemPlan])
          , (T.concat ["items[", idx', "][quantity]"],  [T.pack $ show subscriptionItemQuantity])
          ]
