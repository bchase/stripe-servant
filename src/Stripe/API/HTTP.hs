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

type Id id = Capture "id" id

type Body a = ReqBody '[FormUrlEncoded] a

type ReqHeaders a =
     Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> a

type PaginationQP a =
     QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> a

type GetL    a = PaginationQP (ReqHeaders (Get    '[JSON] (ListResp    a)))
type Get'    a =               ReqHeaders (Get    '[JSON] (ScalarResp  a))
type Post'   a =               ReqHeaders (Post   '[JSON] (ScalarResp  a))
type Delete' a =               ReqHeaders (Delete '[JSON] (DestroyResp a))



---- RESPONSES ----

type RespHeaders a = Headers '[Header "Request-Id" String] a

type ScalarResp   a = RespHeaders              a
type ListResp     a = RespHeaders (ListJSON    a)
type DestroyResp id = RespHeaders (DeleteJSON id)


data ListJSON a = ListJSON
  { listJsonHasMore :: Bool
  , listJsonData    :: a
  } deriving ( Show, Generic, Functor )

data DeleteJSON id = DeleteJSON
  { deleteJsonDeleted :: Bool
  , deleteJsonId      :: id
  } deriving ( Show, Generic, Functor )


$(deriveFromJSON' ''ListJSON)

$(deriveFromJSON' ''DeleteJSON)



---- ACTIONS ----

type List           resp =              PaginatedClient (ListResp    resp)
type Create     req resp =       req -> Client          (ScalarResp  resp)
type Update  id req resp = id -> req -> Client          (ScalarResp  resp)
type Read    id     resp = id ->        Client          (ScalarResp  resp)
type Destroy id          = id ->        Client          (DestroyResp   id)
