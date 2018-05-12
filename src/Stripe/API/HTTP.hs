{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Stripe.API.HTTP where

import           GHC.Generics       (Generic)

import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)
import           Servant.Client     (ClientM)
import           Servant.API        ((:>), QueryParam, Capture, Header, Headers,
                                     ReqBody, FormUrlEncoded, JSON, Get, Post, Delete)

import           Stripe.Types       (StripeAccountId, StripeSecretKey,
                                     StripeVersion, PaginationLimit,
                                     PaginationStartingAfter, PaginationEndingBefore)



---- CLIENT ----

type RunnableStripeClient a = ClientM a

type StripeClient resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> RunnableStripeClient resp

type StripeClientPaginated resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> StripeClient resp



---- REQUESTS ----

type CapId t = Capture "id" t
type RBody t = ReqBody '[FormUrlEncoded] t

type GetListS a = Get    '[JSON] (StripeListResp    a)
type GetShowS a = Get    '[JSON] (StripeScalarResp  a)
type PostS    a = Post   '[JSON] (StripeScalarResp  a)
type DeleteS  a = Delete '[JSON] (StripeDestroyResp a)

type StripeHeaders resp =
     Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> resp

type StripePaginationQueryParams resp =
     QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> resp



---- RESPONSES ----

type StripeScalarResp   a = Headers '[Header "Request-Id" String]                    a
type StripeListResp     a = Headers '[Header "Request-Id" String] (StripeListJSON    a)
type StripeDestroyResp id = Headers '[Header "Request-Id" String] (StripeDeleteJSON id)

data StripeListJSON a = StripeListJSON
  { stripeListJsonObject  :: String
  , stripeListJsonUrl     :: String
  , stripeListJsonHasMore :: Bool
  , stripeListJsonData    :: a
  } deriving ( Show, Generic, Functor )

data StripeDeleteJSON id = StripeDeleteJSON
  { stripeDeleteJsonId      :: id
  , stripeDeleteJsonDeleted :: Bool
  } deriving ( Show, Generic, Functor )

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''StripeListJSON)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 16 } ''StripeDeleteJSON)



---- ACTIONS ----

type ListS           resp =              StripeClientPaginated (StripeListResp    resp)
type CreateS     req resp =       req -> StripeClient          (StripeScalarResp  resp)
type UpdateS  id req resp = id -> req -> StripeClient          (StripeScalarResp  resp)
type ReadS    id     resp = id ->        StripeClient          (StripeScalarResp  resp)
type DestroyS id          = id ->        StripeClient          (StripeDestroyResp   id)
