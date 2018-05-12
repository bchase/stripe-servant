{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stripe.Data.Id where

import qualified Data.Text    as T
import           GHC.Generics (Generic)

import           Data.Aeson   as J
import           Servant.API  (ToHttpApiData (toQueryParam, toUrlPiece))

import           Stripe.Util  (fromJsonString)



newtype AccountId     = AccountId     { unAccountId     :: T.Text } deriving ( Eq, Show, Generic )
newtype BankAccountId = BankAccountId { unBankAccountId :: T.Text } deriving ( Eq, Show, Generic )
newtype CardId        = CardId        { unCardId        :: T.Text } deriving ( Eq, Show, Generic )
newtype ChargeId      = ChargeId      { unChargeId      :: T.Text } deriving ( Eq, Show, Generic )
newtype CustomerId    = CustomerId    { unCustomerId    :: T.Text } deriving ( Eq, Show, Generic )
newtype InvoiceId     = InvoiceId     { unInvoiceId     :: T.Text } deriving ( Eq, Show, Generic )
newtype PlanId        = PlanId        { unPlanId        :: T.Text } deriving ( Eq, Show, Generic )



---- FromJSON INSTANCES ----

instance J.FromJSON AccountId where
  parseJSON = fromJsonString AccountId
instance J.FromJSON BankAccountId where
  parseJSON = fromJsonString BankAccountId
instance J.FromJSON CardId where
  parseJSON = fromJsonString CardId
instance J.FromJSON ChargeId where
  parseJSON = fromJsonString ChargeId
instance J.FromJSON CustomerId where
  parseJSON = fromJsonString CustomerId
instance J.FromJSON InvoiceId where
  parseJSON = fromJsonString InvoiceId
instance J.FromJSON PlanId where
  parseJSON = fromJsonString PlanId
instance J.FromJSON Token where
  parseJSON = fromJsonString Token

instance ToHttpApiData AccountId where
  toQueryParam = unAccountId
instance ToHttpApiData BankAccountId where
  toQueryParam = unBankAccountId
instance ToHttpApiData CardId where
  toQueryParam = unCardId
instance ToHttpApiData ChargeId where
  toQueryParam = unChargeId
instance ToHttpApiData CustomerId where
  toQueryParam = unCustomerId
instance ToHttpApiData InvoiceId where
  toQueryParam = unInvoiceId
instance ToHttpApiData PlanId where
  toQueryParam = unPlanId



-- TODO mv

newtype Token = Token { unToken :: T.Text } deriving (Show, Generic)

instance ToHttpApiData Token where
  toUrlPiece = unToken
