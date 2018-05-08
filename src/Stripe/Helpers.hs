{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Stripe.Helpers
  ( stripeScalar'
  , stripeList'
  , stripeDelete'
  , deriveFromJSON'
  , paginate
  , unpaginated
  -- , deriveToForm
  -- TODO clean up below
  , S
  , StripeConfig (..)
  , scalar
  , list
  , destroy
  , stripe
  , stripeIO
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
-- import qualified Web.Internal.FormUrlEncoded as F
import           Servant.API
import           Servant.Client              (ClientEnv (ClientEnv), Scheme (Https),
                                              BaseUrl (BaseUrl), runClientM)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)

import           Stripe.Types
import           Stripe.Error                (stripeError)


deriveFromJSON' :: TH.Name -> TH.Q [TH.Dec]
deriveFromJSON' name =
  deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop (moduleNameLength name) } name
moduleNameLength :: TH.Name -> Int
moduleNameLength = length . takeWhile (/= '.') . reverse . show
-- deriveToForm :: TH.Name -> (a -> F.Form)
-- deriveToForm name =
--   let len = length . takeWhile (/= '.') . reverse . show $ name
--    in F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop len }


unpaginated :: StripeClientPaginated (StripeListResp a) -> StripeClient (StripeListResp a)
unpaginated = paginate []

paginate :: [PaginationOpt] -> StripeClientPaginated (StripeListResp a) -> StripeClient (StripeListResp a)
paginate pagination clientM =
  let PaginationOpts{paginateBy, paginateStartingAfter, paginateEndingBefore} = buildPagination pagination
   in clientM paginateBy paginateStartingAfter paginateEndingBefore
  where
    buildPagination :: [PaginationOpt] -> PaginationOpts
    buildPagination = foldl set $ PaginationOpts Nothing Nothing Nothing

    set p (PaginateBy            num) = p { paginateBy            = Just . PaginateBy'            $ num }
    set p (PaginateStartingAfter id') = p { paginateStartingAfter = Just . PaginateStartingAfter' $ id' }
    set p (PaginateEndingBefore  id') = p { paginateEndingBefore  = Just . PaginateEndingBefore'  $ id' }


stripeScalar' :: StripeSecretKey -> StripeConnect -> StripeClient (StripeScalarResp a) -> IO (StripeS a)
stripeScalar' = stripeRunner stripeScalarFromResp where
  stripeScalarFromResp (Headers a hs) = StripeScalar (getReqId hs) a

stripeDelete' :: StripeSecretKey -> StripeConnect -> StripeClient (StripeDestroyResp id) -> IO (StripeD id)
stripeDelete' = stripeRunner stripeDeleteFromResp where
  stripeDeleteFromResp (Headers StripeDeleteJSON{stripeDeleteJsonId, stripeDeleteJsonDeleted} hs) =
    StripeDestroy (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted

stripeList' :: StripeSecretKey -> StripeConnect -> [PaginationOpt] -> StripeClientPaginated (StripeListResp a) -> IO (StripeL a)
stripeList' secretKey connect pagination clientM =
  stripeRunner stripeListFromResp secretKey connect clientM'
  where
    PaginationOpts{..} = buildPagination pagination

    buildPagination :: [PaginationOpt] -> PaginationOpts
    buildPagination = foldl updatePagination emptyPagination
      where
        emptyPagination = PaginationOpts Nothing Nothing Nothing
        updatePagination p' (PaginateBy            num) = p' { paginateBy            = Just . PaginateBy'            $ num }
        updatePagination p' (PaginateStartingAfter id') = p' { paginateStartingAfter = Just . PaginateStartingAfter' $ id' }
        updatePagination p' (PaginateEndingBefore  id') = p' { paginateEndingBefore  = Just . PaginateEndingBefore'  $ id' }

    clientM' = clientM paginateBy paginateStartingAfter paginateEndingBefore

    stripeListFromResp (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
      StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData

stripeRunner :: (a -> b) -> StripeSecretKey -> StripeConnect -> StripeClient a -> IO (Either StripeFailure b)
stripeRunner respToData secretKey connect clientM =
  clientEnv >>= runClientM clientM' >>= return . either (Left . stripeError) (Right . respToData)
  where
    clientM' = clientM (toMaybe connect) (Just secretKey) (Just version)

    toMaybe  WithoutConnect   = Nothing
    toMaybe (WithConnect id') = Just id'

    version = StripeVersion'2017'08'15

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url


getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""


---- WIP ----

-- data StripeMeta -- TODO better approach?
--   = StripeScalar'
--   | StripeDestroy' Bool -- { stripeDeleted :: Bool } -- TODO throwError instead of Bool?
--   | StripeList' Bool
--   deriving ( Show, Generic )
-- hasMore :: Stripe' a -> Bool -- TODO ???
-- hasMore = hasMore' . stripeMeta where
--   hasMore' (StripeList' True) = True
--   hasMore' _                  = False
-- deleted :: Stripe' a -> Bool -- TODO ???
-- deleted = deleted' . stripeMeta where
--   deleted' (StripeDestroy' True) = True
--   deleted' _                     = False
--
-- data Stripe' a = Stripe'
--   { stripeRequestId :: RequestId
--   , stripeMeta      :: StripeMeta
--   , stripeData      :: a
--   } deriving ( Show, Generic )
--
-- -- TODO rm
-- stripeS :: StripeScalar a -> Stripe' a
-- stripeS StripeScalar{..} =
--   Stripe' stripeScalarRequestId StripeScalar' stripeScalarData
-- stripeL :: StripeList a -> Stripe' a
-- stripeL StripeList{..} =
--   let meta = StripeList' stripeListHasMore
--    in Stripe' stripeListRequestId meta stripeListData
-- stripeD :: StripeDestroy id -> Stripe' id
-- stripeD StripeDestroy{..} =
--   let meta = StripeDestroy' stripeDestroyDeleted
--    in Stripe' stripeDestroyRequestId meta stripeDestroyId


-- class ( StripeData d ) => StripeCall a where
--   stripe' ::


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


-- newtype StripeResp a = StripeResp { unStripeResp :: Headers '[Header "Request-Id" String] a }
--
-- class ToStripeData r where
--   toStripeData :: ( StripeData d ) => r a -> d a
--
-- instance ToStripeData StripeResp where
--   toStripeData (StripeResp (Headers a hs)) = StripeScalar (getReqId hs) a
--   -- toStripeData (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
--   --   StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData
--   -- toStripeData (Headers StripeDeleteJSON{stripeDeleteJsonId, stripeDeleteJsonDeleted} hs) =
--   --   StripeDestroy (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted
--   toStripeData _ = undefined -- TODO

-- instance ToStripeData (StripeResp StripeListJSON) where
--   foo _ = True
-- instance StripeJSON StripeScalarResp where
--   toStripeData (Headers a hs) = StripeScalar (getReqId hs) a
--
-- instance StripeJSON StripeListJSON where
--   toStripeData (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
--     StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData


-- type StripeClient resp =
--      Maybe StripeAccountId
--   -> Maybe StripeSecretKey
--   -> Maybe StripeVersion
--   -> RunnableStripeClient resp
--
-- type StripeClientPaginated resp =
--      Maybe PaginationLimit
--   -> Maybe PaginationStartingAfter
--   -> Maybe PaginationEndingBefore
--   -> StripeClient resp
--
-- s :: StripeConnect ->                    StripeClient          (StripeScalarResp   a) -> S (StripeScalar   a)
-- d :: StripeConnect ->                    StripeClient          (StripeDestroyResp id) -> S (StripeDestroy id)
-- l :: StripeConnect -> [PaginationOpt] -> StripeClientPaginated (StripeListResp     a) -> S (StripeList     a)

-- newtype StripeCall params = StripeCall { callStripe :: ReaderT cfg mt a }
--
-- TODO make this polymorphic over scalar/destroy/list
s :: ( StripeData d ) => (json -> d a) -> StripeConnect -> StripeClient json -> S (d a)
s = undefined

newtype S a = S { runStripe :: ReaderT StripeConfig ( ExceptT StripeFailure IO ) a }
  deriving ( Functor, Applicative, Monad, MonadReader StripeConfig, MonadError StripeFailure, MonadIO ) -- TODO rm `MonadIO`?

data StripeConfig = StripeConfig
  { stripeVersion   :: StripeVersion
  , stripeSecretKey :: StripeSecretKey
  }

stripeIO :: StripeConfig -> S a -> IO (Either StripeFailure a)
stripeIO cfg = runExceptT . flip runReaderT cfg . runStripe


scalar :: StripeConnect -> StripeClient (StripeScalarResp a) -> S (StripeScalar a)
scalar = stripe stripeScalarFromResp where
  stripeScalarFromResp :: StripeScalarResp a -> StripeScalar a
  stripeScalarFromResp (Headers a hs) = StripeScalar (getReqId hs) a

destroy :: StripeConnect -> StripeClient (StripeDestroyResp id) -> S (StripeDestroy id)
destroy = stripe stripeDeleteFromResp where
  stripeDeleteFromResp :: StripeDestroyResp a -> StripeDestroy a
  stripeDeleteFromResp (Headers StripeDeleteJSON{stripeDeleteJsonId, stripeDeleteJsonDeleted} hs) =
    StripeDestroy (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted

list :: StripeConnect -> StripeClient (StripeListResp a) -> S (StripeList a)
list = stripe stripeListFromResp where
  stripeListFromResp :: StripeListResp a -> StripeList a
  stripeListFromResp (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
    StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData

stripe :: (a -> b) -> StripeConnect -> StripeClient a -> S b
stripe respToData connect clientM = do
  (ver, key) <- asks $ (,) <$> stripeVersion <*> stripeSecretKey

  let clientM' = clientM (toMaybe connect) (Just key) (Just ver)
  eStripe <- liftIO $ runClientM clientM' =<< clientEnv

  either (throwError . stripeError) (return . respToData) eStripe

  where
    toMaybe  WithoutConnect    = Nothing
    toMaybe (WithConnect acct) = Just acct

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url
