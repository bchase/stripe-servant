{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripe.API.Request.Card where

import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import qualified Web.Internal.FormUrlEncoded as F

import           Stripe.Data.Id (Token)



data CardCreateReq = CardCreateReq
  { cardCreateSource :: Token
  } deriving ( Generic )

data CardUpdateReq = CardUpdateReq
  { cardUpdateExpMonth :: Maybe Int
  , cardUpdateExpYear  :: Maybe Int
  } deriving ( Generic )



---- HELPERS ----

cardCreateReq :: Token -> CardCreateReq
cardCreateReq token = CardCreateReq token

cardUpdateReq :: CardUpdateReq
cardUpdateReq = CardUpdateReq Nothing Nothing



---- ToForm INSTANCES ----

instance F.ToForm CardCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }

instance F.ToForm CardUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 10 }
