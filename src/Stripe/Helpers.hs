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
  , paginate
  , unpaginated
  , deriveFromJSON'
  ) where

import           Data.Either                 (either)
import           Control.Monad.Except        (throwError)
import           Control.Monad.Reader        (asks, liftIO)
import           Language.Haskell.TH.Syntax  as TH

import           Data.Aeson.Casing           (snakeCase)
import           Data.Aeson.TH               (Options (..), defaultOptions, deriveFromJSON)
import           Servant.API
import           Servant.Client              (ClientEnv (ClientEnv), Scheme (Https),
                                              BaseUrl (BaseUrl), runClientM)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)

import           Stripe.API.HTTP
import           Stripe.Types
import           Stripe.Error                (stripeError)



---- StripeClient RUNNERS ----

stripeS :: StripeConnect -> StripeClient (StripeScalarResp a) -> Stripe a
stripeS connect = fmap stripeData . stripeS' connect
stripeL :: StripeConnect -> StripeClient (StripeListResp a) -> Stripe a
stripeL connect = fmap stripeData . stripeL' connect
stripeD :: StripeConnect -> StripeClient (StripeDestroyResp a) -> Stripe a
stripeD connect = fmap stripeData . stripeD' connect

stripeS' :: StripeConnect -> StripeClient (StripeScalarResp a) -> Stripe (StripeScalar a)
stripeS' connect = scalar . stripe connect
stripeL' :: StripeConnect -> StripeClient (StripeListResp a) -> Stripe (StripeList a)
stripeL' connect = list . stripe connect
stripeD' :: StripeConnect -> StripeClient (StripeDestroyResp a) -> Stripe (StripeDestroy a)
stripeD' connect = destroyed . stripe connect



---- PAGINATION HELPERS ----

unpaginated :: StripeClientPaginated (StripeListResp a) -> StripeClient (StripeListResp a)
unpaginated = paginate []

paginate :: [PaginationOpt] -> StripeClientPaginated (StripeListResp a) -> StripeClient (StripeListResp a)
paginate pagination clientM =
  let PaginationOpts{paginateBy, paginateStartingAfter, paginateEndingBefore} = buildPagination pagination
   in clientM paginateBy paginateStartingAfter paginateEndingBefore
  where
    buildPagination :: [PaginationOpt] -> PaginationOpts
    buildPagination = foldl set $ PaginationOpts Nothing Nothing Nothing

    set p (By            num) = p { paginateBy            = Just . By'            $ num }
    set p (StartingAfter id') = p { paginateStartingAfter = Just . StartingAfter' $ id' }
    set p (EndingBefore  id') = p { paginateEndingBefore  = Just . EndingBefore'  $ id' }



---- JSON HELPERS ----

deriveFromJSON' :: TH.Name -> TH.Q [TH.Dec]
deriveFromJSON' n = deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop (moduleNameLength n) } n where
  moduleNameLength = length . takeWhile (/= '.') . reverse . show



---- PRIVATE ----

stripe :: StripeConnect -> StripeClient a -> Stripe a
stripe connect client = do
  (ver, key) <- asks $ (,) <$> stripeVersion <*> stripeSecretKey

  let client' = client (toMaybe connect) (Just key) (Just ver)
  eStripe <- liftIO $ runClientM client' =<< clientEnv

  either (throwError . stripeError) return eStripe

  where
    toMaybe  WithoutConnect    = Nothing
    toMaybe (WithConnect acct) = Just acct

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url


scalar :: Stripe (StripeScalarResp a) -> Stripe (StripeScalar a)
scalar = fmap (\(Headers x hs) -> StripeScalar (getReqId hs) x)

list :: Stripe (StripeListResp a) -> Stripe (StripeList a)
list = fmap (\(Headers StripeListJSON{..} hs) -> StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData)

destroyed :: Stripe (StripeDestroyResp id) -> Stripe (StripeDestroy id)
destroyed = fmap (\(Headers StripeDeleteJSON{..} hs) -> StripeDestroy (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted)


getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""
