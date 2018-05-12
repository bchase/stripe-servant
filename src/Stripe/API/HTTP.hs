{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Stripe.API.HTTP where

import           GHC.Generics       (Generic)

import           Servant.Client     (ClientM)
import           Servant.API        ((:>), QueryParam, Capture, Header, Headers,
                                     ReqBody, FormUrlEncoded, JSON, Get, Post, Delete)

import           Stripe.Util        (deriveFromJSON')
import           Stripe.Types       (StripeAccountId, StripeSecretKey,
                                     StripeVersion, PaginationLimit,
                                     PaginationStartingAfter, PaginationEndingBefore)



---- CLIENT ----

type Client resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> ClientM resp

type PaginatedClient resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> Client resp



---- REQUESTS ----

type Id   t = Capture "id" t
type Body t = ReqBody '[FormUrlEncoded] t

type GetJL   a = Get    '[JSON] (ListResp    a)
type GetJS   a = Get    '[JSON] (ScalarResp  a)
type PostJ   a = Post   '[JSON] (ScalarResp  a)
type DeleteJ a = Delete '[JSON] (DestroyResp a)

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

type ScalarResp   a = Headers '[Header "Request-Id" String]              a
type ListResp     a = Headers '[Header "Request-Id" String] (ListJSON    a)
type DestroyResp id = Headers '[Header "Request-Id" String] (DeleteJSON id)


data ListJSON a = ListJSON
  { listJsonObject  :: String
  , listJsonUrl     :: String
  , listJsonHasMore :: Bool
  , listJsonData    :: a
  } deriving ( Show, Generic, Functor )

data DeleteJSON id = DeleteJSON
  { deleteJsonId      :: id
  , deleteJsonDeleted :: Bool
  } deriving ( Show, Generic, Functor )


$(deriveFromJSON' ''ListJSON)

$(deriveFromJSON' ''DeleteJSON)



---- ACTIONS ----

type List           resp =              PaginatedClient (ListResp    resp)
type Create     req resp =       req -> Client          (ScalarResp  resp)
type Update  id req resp = id -> req -> Client          (ScalarResp  resp)
type Read    id     resp = id ->        Client          (ScalarResp  resp)
type Destroy id          = id ->        Client          (DestroyResp   id)
