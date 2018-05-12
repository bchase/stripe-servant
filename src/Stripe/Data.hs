{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stripe.Data where

import           GHC.Generics       (Generic)



data Resp a = Resp
  { stripeRequestId :: String
  , stripeMetadata  :: RespMetadata
  , stripeData      :: a
  } deriving ( Show, Generic, Functor )

data RespMetadata
  = ScalarMeta
  | ListMeta    { listHasMore    :: Bool }
  | DestroyMeta { destroyDeleted :: Bool }
  deriving ( Show, Generic )


data StripeScalar a = StripeScalar
  { stripeScalarRequestId :: String
  , stripeScalarData      :: a
  } deriving ( Show, Generic, Functor )

data StripeList a = StripeList
  { stripeListRequestId :: String
  , stripeListHasMore   :: Bool
  , stripeListData      :: a
  } deriving ( Show, Generic, Functor )

data StripeDestroy id = StripeDestroy
  { stripeDestroyRequestId :: String
  , stripeDestroyDeleted   :: Bool
  , stripeDestroyId        :: id
  } deriving ( Show, Generic, Functor )


-- class StripeData s where
--   stripeReqId :: s a -> String
--   stripeData  :: s a -> a
--
-- instance StripeData StripeScalar where
--   stripeReqId = stripeScalarRequestId
--   stripeData  = stripeScalarData
-- instance StripeData StripeList where
--   stripeReqId = stripeListRequestId
--   stripeData  = stripeListData
-- instance StripeData StripeDestroy where
--   stripeReqId = stripeDestroyRequestId
--   stripeData  = stripeDestroyId
