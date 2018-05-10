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
  ( deriveFromJSON'
  , paginate
  , unpaginated
  , StripeData (stripeReqId, stripeData)
  , S
  , StripeConfig (..)
  , stripeIO
  , stripe
  , scalar
  , list
  , destroyed
  ) where

import qualified Data.Text                   as T
import           Data.Either                 (either)
import           Control.Monad.Except        (MonadError, throwError,
                                              ExceptT, runExceptT)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT, asks,
                                              MonadIO, liftIO)
import           GHC.Generics                (Generic)
import           Language.Haskell.TH.Syntax  as TH

import           Data.Aeson.Casing           (snakeCase)
import           Data.Aeson.TH               (Options (..), defaultOptions, deriveFromJSON)
import           Servant.API
import           Servant.Client              (ClientEnv (ClientEnv), Scheme (Https),
                                              BaseUrl (BaseUrl), runClientM)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)

import           Stripe.Types
import           Stripe.Error                (stripeError)



deriveFromJSON' :: TH.Name -> TH.Q [TH.Dec]
deriveFromJSON' n = deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop (moduleNameLength n) } n where
  moduleNameLength = length . takeWhile (/= '.') . reverse . show



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


class StripeData s where
  stripeReqId :: s a -> RequestId
  stripeData  :: s a -> a

instance StripeData StripeScalar where
  stripeReqId = stripeScalarRequestId
  stripeData  = stripeScalarData
instance StripeData StripeList where
  stripeReqId = stripeListRequestId
  stripeData  = stripeListData
instance StripeData StripeDestroy where
  stripeReqId = stripeDestroyRequestId
  stripeData  = stripeDestroyId



newtype S a = S { runStripe :: ReaderT StripeConfig ( ExceptT StripeFailure IO ) a }
  deriving ( Functor, Applicative, Monad, MonadReader StripeConfig, MonadError StripeFailure, MonadIO )

data StripeConfig = StripeConfig
  { stripeVersion   :: StripeVersion
  , stripeSecretKey :: StripeSecretKey
  }

stripeIO :: StripeConfig -> S a -> IO (Either StripeFailure a)
stripeIO cfg = runExceptT . flip runReaderT cfg . runStripe

stripe :: StripeConnect -> StripeClient a -> S a
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


scalar :: S (StripeScalarResp a) -> S (StripeScalar a)
scalar = fmap (\(Headers x hs) -> StripeScalar (getReqId hs) x)

list :: S (StripeListResp a) -> S (StripeList a)
list = fmap (\(Headers StripeListJSON{..} hs) -> StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData)

destroyed :: S (StripeDestroyResp id) -> S (StripeDestroy id)
destroyed = fmap (\(Headers StripeDeleteJSON{..} hs) -> StripeDestroy (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted)



getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""
