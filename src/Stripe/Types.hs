{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Stripe.Types where

import           GHC.Generics              (Generic)

import qualified Data.Aeson                as J


type RequestId = String -- TODO newtype ?

newtype ResourceId = ResourceId String
  deriving (Show, Generic, J.FromJSON)
