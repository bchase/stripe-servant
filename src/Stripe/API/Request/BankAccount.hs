{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.API.Request.BankAccount where

import           GHC.Generics (Generic)

import           Data.Aeson.Casing           (snakeCase)
import           Servant.API                 (ToHttpApiData (toQueryParam))
import           Web.Internal.FormUrlEncoded (ToForm (..), toForm, genericToForm,
                                              defaultFormOptions, fieldLabelModifier)

import           Stripe.Data.Id (Token)



data BankAccountCreate = BankAccountCreate
  { bankAccountCreateSource :: Token
  } deriving (Generic)

data BankAccountUpdateReq = BankAccountUpdateReq
  { bankAccountUpdateAccountHolderName :: Maybe String
  , bankAccountUpdateAccountHolderType :: Maybe String
  } deriving (Generic)

data BankAccountVerifyReq = BankAccountVerifyReq
  { bankAccountVerifyAmount1 :: Int
  , bankAccountVerifyAmount2 :: Int
  } deriving (Generic)



---- HELPERS ----

bankAccountCreateReq :: Token -> BankAccountCreate
bankAccountCreateReq token = BankAccountCreate token

bankAccountUpdateReq :: BankAccountUpdateReq
bankAccountUpdateReq = BankAccountUpdateReq Nothing Nothing




---- ToForm INSTANCES ----

instance ToForm BankAccountCreate where
  toForm = (\n -> genericToForm $ defaultFormOptions { fieldLabelModifier = snakeCase . drop (length . reverse . takeWhile (/= '.') . reverse . show $ n) }) ''BankAccountCreate -- TODO DUP1 TH deriveToForm

instance ToForm BankAccountUpdateReq where
  toForm = genericToForm $ defaultFormOptions { fieldLabelModifier = snakeCase . drop 17 }

instance ToForm BankAccountVerifyReq where
  toForm req =
    [ ("amounts[]", toQueryParam . bankAccountVerifyAmount1 $ req)
    , ("amounts[]", toQueryParam . bankAccountVerifyAmount2 $ req)
    ]
