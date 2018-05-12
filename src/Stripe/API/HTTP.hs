{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO

module Stripe.API.HTTP where

import           GHC.Generics       (Generic)

import           Servant.Client     (ClientM)
import           Servant.API        ((:>), QueryParam, Capture, Header (..), Headers (..), HList (..),
                                     ReqBody, FormUrlEncoded, JSON, Get, Post, Delete)

import           Stripe.Util        (deriveFromJSON')
import           Stripe.Data.Id     (AccountId)
import           Stripe.Data        (Resp (..), RespMetadata (..))
import           Stripe.Types       (SecretKey, Version, PaginationLimit,
                                     PaginationStartingAfter, PaginationEndingBefore)



---- CLIENT ----

type Client resp =   -- `Maybe`s due to `Servant.Header`
     Maybe SecretKey -- REQUIRED (own secret key)
  -> Maybe Version   --          (set via Config)
  -> Maybe AccountId -- OPTIONAL (Connect account)
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
     Header "Authorization"  SecretKey
  :> Header "Stripe-Version" Version
  :> Header "Stripe-Account" AccountId
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

type ScalarResp'  a = RespHeaders (ScalarJSON  a)

type ScalarResp   a = RespHeaders              a
type ListResp     a = RespHeaders (ListJSON    a)
type DestroyResp id = RespHeaders (DeleteJSON id)


class ToData f where
  toData     :: f a -> a
  toMetadata :: f a -> RespMetadata

instance ToData ScalarJSON where
  toData = scalarJsonData
  toMetadata _ = ScalarMeta

instance ToData ListJSON where
  toData = listJsonData
  toMetadata ListJSON{..} = ListMeta listJsonHasMore

instance ToData DeleteJSON where
  toData = deleteJsonId
  toMetadata DeleteJSON{..} = DestroyMeta deleteJsonDeleted


foo :: Headers '[] a -> Resp a
foo = undefined
getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""



data ScalarJSON a = ScalarJSON
  { scalarJsonData :: a
  } deriving ( Show, Generic, Functor )

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
