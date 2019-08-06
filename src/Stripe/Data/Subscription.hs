{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Subscription where

import           GHC.Generics   (Generic)

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Data.Id (SubscriptionId)
import           Stripe.Types   (Metadata)
-- import           Stripe.Types   (Time (..), Interval (..), Price (..),
--                                  CurrencyCode (..), Metadata (..))



data Subscription = Subscription
  { subscriptionId                  :: SubscriptionId
  , subscriptionMetadata            :: Metadata
  } deriving ( Show, Generic )



---- HELPERS ----

---- FromJSON INSTANCES ----

$(deriveFromJSON' ''Subscription)
