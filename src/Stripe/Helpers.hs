{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Stripe.Helpers
  ( stripeScalar'
  , stripeList'
  , stripeDelete'
  , deriveFromJSON'
  -- , deriveToForm
  ) where

import           Data.Either                 (either)
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


stripeScalar' :: StripeSecretKey -> StripeConnect -> StripeClient (StripeScalarResp a) -> IO (StripeS a)
stripeScalar' = stripeRunner stripeScalarFromResp
  where
    stripeScalarFromResp (Headers a hs) = StripeScalar (getReqId hs) a

stripeDelete' :: StripeSecretKey -> StripeConnect -> StripeClient (StripeDestroyResp id) -> IO (StripeD id)
stripeDelete' = stripeRunner stripeDeleteFromResp
  where
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
        updatePagination p' p =
          case p of
            PaginateBy num            -> p' { paginateBy            = Just . PaginateBy'            $ num }
            PaginateStartingAfter id' -> p' { paginateStartingAfter = Just . PaginateStartingAfter' $ id' }
            PaginateEndingBefore id'  -> p' { paginateEndingBefore  = Just . PaginateEndingBefore'  $ id' }

    clientM' = clientM paginateBy paginateStartingAfter paginateEndingBefore

    stripeListFromResp (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
      StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData

stripeRunner :: (a -> b) -> StripeSecretKey -> StripeConnect -> StripeClient a -> IO (Either StripeFailure b)
stripeRunner respToData secretKey connect clientM =
  clientEnv >>= runClientM clientM' >>= return . either (Left . stripeError) (Right . respToData)
  where
    clientM' = clientM (connectToMaybe connect) (Just secretKey) (Just version)

    version = StripeVersion'2017'08'15

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url

    connectToMaybe s = -- TODO case in func param
      case s of
        WithoutConnect  -> Nothing
        WithConnect id' -> Just id'

getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""
