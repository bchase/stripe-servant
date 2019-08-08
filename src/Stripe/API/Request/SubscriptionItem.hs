{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.API.Request.SubscriptionItem
  ( SubscriptionItemCreateReq (..)
  , subItem
  , subItem'

  ) where

-- import Data.Text (Text)
import GHC.Generics (Generic)
import Stripe.Data.Id (PlanId)
-- import Stripe.Types (Time, Metadata, addMetadataToForm)

-- import qualified Data.Text as T
import qualified Servant.API as S



data SubscriptionItemCreateReq = SubscriptionItemCreateReq
  { subscriptionItemCreatePlan     :: PlanId
  , subscriptionItemCreateQuantity :: Int -- NOTE: required, ie >0
  } deriving ( Show, Generic )



---- HELPERS ----

subItem :: PlanId -> SubscriptionItemCreateReq
subItem plan = subItem' plan 1

subItem' :: PlanId -> Int -> SubscriptionItemCreateReq
subItem' = SubscriptionItemCreateReq



---- INSTANCES ----

instance S.ToHttpApiData SubscriptionItemCreateReq where
  toQueryParam _ = "" -- TODO ... handling via `addItemsToForm` for `SubscriptionCreateReq`
