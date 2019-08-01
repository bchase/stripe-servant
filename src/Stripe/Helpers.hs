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
  , stripe
  , stripe'
  , paginate
  , unpaginated
  ) where

import           Data.Either                 (either)
import           Control.Monad.Except        (throwError)
import           Control.Monad.Reader        (asks, liftIO)

import           Servant.Client              (ClientEnv (ClientEnv), Scheme (Https),
                                              BaseUrl (BaseUrl), runClientM)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)

import           Stripe.API.HTTP
import           Stripe.Types
import           Stripe.Error




---- Client RUNNERS ----

stripe  :: ( RespBody rb ) => Connect -> Client (RespHeaders (rb a)) -> Stripe a
stripe connect = fmap stripeData . stripe' connect

stripe' :: ( RespBody rb ) => Connect -> Client (RespHeaders (rb a)) -> Stripe (Resp a)
stripe' connect client = do
  (ver, key) <- asks $ (,) <$> configVersion <*> configSecretKey

  let client' = client (Just key) (Just ver) (toMaybe connect)
  eStripe <- liftIO $ runClientM client' =<< clientEnv

  either (throwError . stripeError) (return . toResp) eStripe

  where
    toMaybe  WithoutConnect    = Nothing
    toMaybe (WithConnect acct) = Just acct

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url Nothing




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
