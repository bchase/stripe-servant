{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripe.API.Request.Customer where

import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import qualified Web.Internal.FormUrlEncoded as F

import           Stripe.Data.Id (Token)



data CustomerCreateReq = CustomerCreateReq
  { customerCreateSource      :: Token
  , customerCreateEmail       :: Maybe String
  , customerCreateDescription :: Maybe String
  } deriving ( Generic )

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving ( Generic )



---- HELPERS ----

customerCreateReq :: Token -> CustomerCreateReq
customerCreateReq token = CustomerCreateReq token Nothing Nothing

customerUpdateReq :: CustomerUpdateReq
customerUpdateReq = CustomerUpdateReq Nothing Nothing



---- ToForm INSTANCES ----

instance F.ToForm CustomerCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }

instance F.ToForm CustomerUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
