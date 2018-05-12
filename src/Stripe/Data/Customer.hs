{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Customer where

import           GHC.Generics   (Generic)

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Data.Id (CustomerId)



data Customer = Customer
  { customerId          :: CustomerId
  , customerDescription :: Maybe String
  , customerEmail       :: Maybe String
  } deriving ( Show, Generic )



---- FromJSON INSTANCES ----

$(deriveFromJSON' ''Customer)
