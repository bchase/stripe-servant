{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Stripe.Helpers
  ( stripeIO
  , stripeS
  , stripeL
  , stripeD
  , stripeS'
  , stripeL'
  , stripeD'
  , stripe'
  , paginate
  , unpaginated
  ) where

import           Data.Either                 (either)
import           Control.Monad.Except        (throwError)
import           Control.Monad.Reader        (asks, liftIO)

import           Servant.API
import           Servant.Client              (ClientEnv (ClientEnv), Scheme (Https),
                                              BaseUrl (BaseUrl), runClientM)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)

import           Stripe.API.HTTP             hiding (getReqId)
import           Stripe.Types
import           Stripe.Error                (stripeError)
import           Stripe.Data                 (StripeScalar (..), StripeList (..),
                                              StripeDestroy (..), Resp (..))



---- Client RUNNERS ----

stripe' :: ( ToData d ) => Connect -> Client (ScalarResp (d a)) -> Stripe (Resp a)
stripe' connect client = do
  (ver, key) <- asks $ (,) <$> configVersion <*> configSecretKey

  let client' = client (Just key) (Just ver) (toMaybe connect)
  eStripe <- liftIO $ runClientM client' =<< clientEnv

  either (throwError . stripeError) (return . toResp) eStripe

  where
    toResp (Headers x hs) = Resp (getReqId hs) (toMetadata x) (toData x)

    toMaybe  WithoutConnect    = Nothing
    toMaybe (WithConnect acct) = Just acct

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url


stripeS :: Connect -> Client (ScalarResp a) -> Stripe a
stripeS connect = fmap stripeScalarData . stripeS' connect
stripeL :: Connect -> Client (ListResp a) -> Stripe a
stripeL connect = fmap stripeListData . stripeL' connect
stripeD :: Connect -> Client (DestroyResp a) -> Stripe a
stripeD connect = fmap stripeDestroyId . stripeD' connect

stripeS' :: Connect -> Client (ScalarResp a) -> Stripe (StripeScalar a)
stripeS' connect = scalar . stripe connect
stripeL' :: Connect -> Client (ListResp a) -> Stripe (StripeList a)
stripeL' connect = list . stripe connect
stripeD' :: Connect -> Client (DestroyResp a) -> Stripe (StripeDestroy a)
stripeD' connect = destroyed . stripe connect



---- PAGINATION HELPERS ----

unpaginated :: PaginatedClient (ListResp a) -> Client (ListResp a)
unpaginated = paginate []

paginate :: [PaginationOpt] -> PaginatedClient (ListResp a) -> Client (ListResp a)
paginate pagination clientM =
  let PaginationOpts{paginateBy, paginateStartingAfter, paginateEndingBefore} = buildPagination pagination
   in clientM paginateBy paginateStartingAfter paginateEndingBefore
  where
    buildPagination :: [PaginationOpt] -> PaginationOpts
    buildPagination = foldl set $ PaginationOpts Nothing Nothing Nothing

    set p (By            num) = p { paginateBy            = Just . By'            $ num }
    set p (StartingAfter id') = p { paginateStartingAfter = Just . StartingAfter' $ id' }
    set p (EndingBefore  id') = p { paginateEndingBefore  = Just . EndingBefore'  $ id' }



---- PRIVATE ----

stripe :: Connect -> Client a -> Stripe a
stripe connect client = do
  (ver, key) <- asks $ (,) <$> configVersion <*> configSecretKey

  let client' = client (Just key) (Just ver) (toMaybe connect)
  eStripe <- liftIO $ runClientM client' =<< clientEnv

  either (throwError . stripeError) return eStripe

  where
    toMaybe  WithoutConnect    = Nothing
    toMaybe (WithConnect acct) = Just acct

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url


scalar :: Stripe (ScalarResp a) -> Stripe (StripeScalar a)
scalar = fmap (\(Headers x hs) -> StripeScalar (getReqId hs) x)

list :: Stripe (ListResp a) -> Stripe (StripeList a)
list = fmap (\(Headers ListJSON{..} hs) -> StripeList (getReqId hs) listJsonHasMore listJsonData)

destroyed :: Stripe (DestroyResp id) -> Stripe (StripeDestroy id)
destroyed = fmap (\(Headers DeleteJSON{..} hs) -> StripeDestroy (getReqId hs) deleteJsonDeleted deleteJsonId)


getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""
