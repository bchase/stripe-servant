{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Stripe where

import           Prelude                     hiding (ReadS)
import qualified Data.Text                   as T
import           Data.Proxy                  (Proxy (Proxy))
import           GHC.Generics                (Generic)

-- import           GHC.TypeLits                (Symbol)
import           Data.Aeson                  as J
import           Data.Aeson.Casing           (snakeCase)
import           Data.Aeson.TH               (Options (..), defaultOptions, deriveFromJSON)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)
import           Web.Internal.FormUrlEncoded (ToForm)
import           Servant.API
import           Servant.Client              (ClientM, ClientEnv (ClientEnv),
                                              Scheme (Https), BaseUrl (BaseUrl),
                                              ServantError (..), runClientM, client)

import           Stripe.Types                (ResourceId (ResourceId), RequestId)
import           Stripe.Error                (StripeFailure (..), stripeError)


-- TODO
-- \ * errors -- TODO add status code `Int` to `StripeDecodeFailure`
-- X * pagination
--   * metadata
-- X ? request IDs
--   ? idempotency
--   ! flesh out data types
--   ! (define needed and) add endpoints
--   - mv things to Stripe.Client/Stripe.Data/etc.
--   - Persistent-style TH for type definitions/JSON
--   - change `String` to `Text`
--
-- TODO
--   - nested lists
--   - hardcode query param?
--   e.g.
--   GET https://api.stripe.com/v1/customers/{CUSTOMER_ID}/sources?object=bank_account


newtype StripeAccountId = StripeAccountId String

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

---- PAGINATION ----

---- PUBLIC PAGINATION ----

data Pagination
  = PaginateBy Int -- Nat
  | PaginateStartingAfter ResourceId
  | PaginateEndingBefore ResourceId

---- PRIVATE PAGINATION ----

newtype PaginationLimit         = PaginateBy' Int                   deriving (Generic)
newtype PaginationStartingAfter = PaginateStartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = PaginateEndingBefore' ResourceId  deriving (Generic)

data Pagination' = Pagination'
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

buildPagination :: [Pagination] -> Pagination'
buildPagination = foldl updatePagination emptyPagination
  where
    emptyPagination = Pagination' Nothing Nothing Nothing
    updatePagination p' p =
      case p of
        PaginateBy num            -> p' { paginateBy            = Just . PaginateBy'            $ num }
        PaginateStartingAfter id' -> p' { paginateStartingAfter = Just . PaginateStartingAfter' $ id' }
        PaginateEndingBefore id'  -> p' { paginateEndingBefore  = Just . PaginateEndingBefore'  $ id' }

instance ToHttpApiData PaginationLimit where
  toUrlPiece (PaginateBy' num) = T.pack . show $ num
instance ToHttpApiData PaginationStartingAfter where
  toUrlPiece (PaginateStartingAfter' (ResourceId id')) = T.pack . show $ id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (PaginateEndingBefore' (ResourceId id')) = T.pack . show $ id'

---- END PAGINATION ----



---- STRIPE API DATA TYPES ----

newtype ChargeId   = ChargeId   { unChargeId   :: T.Text } deriving (Show, Generic)
newtype CustomerId = CustomerId { unCustomerId :: T.Text } deriving (Show, Generic)

instance ToHttpApiData ChargeId where
  toQueryParam = unChargeId
instance J.FromJSON ChargeId where
  parseJSON (J.String str) = return . ChargeId $ str
  parseJSON _ = mempty
instance ToHttpApiData CustomerId where
  toQueryParam = unCustomerId
instance J.FromJSON CustomerId where
  parseJSON (J.String str) = return . CustomerId $ str
  parseJSON _ = mempty

newtype Token = Token { unToken :: String } deriving (Show, Generic)
-- instance J.ToJSON Token
instance ToHttpApiData Token where
  toUrlPiece = T.pack . unToken


data CustomerCreateReq = CustomerCreateReq
  { source      :: Token
  , email       :: Maybe String
  , description :: Maybe String
  } deriving (Generic)
-- data CustomerCreateReq = CustomerCreateReq
--   { customerCreateToken       :: Token
--   , customerCreateEmail       :: Maybe String
--   , customerCreateDescription :: Maybe String
--   } deriving (Generic)
-- $(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerCreateReq)
instance ToForm CustomerCreateReq
minCustomerCreateReq :: Token -> CustomerCreateReq
minCustomerCreateReq token = CustomerCreateReq token Nothing Nothing

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving (Generic)
-- $(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerUpdateReq)
instance ToForm CustomerUpdateReq

data Charge = Charge
  { chargeId       :: ChargeId
  , chargeAmount   :: Int
  , chargeCurrency :: String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 6 } ''Charge)

data Customer = Customer
  { customerId          :: CustomerId
  , customerDescription :: Maybe String
  , customerEmail       :: Maybe String
  } deriving (Show, Generic)
$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 8 } ''Customer)

---- STRIPE API ENDPOINTS ----

type API =
  CustomerCreate :<|> CustomerRead :<|> CustomerUpdate :<|> CustomerDestroy :<|> CustomerList

type CapId t = Capture "id" t
type RBody t = ReqBody '[FormUrlEncoded] t

type GetListS a = Get    '[JSON] (StripeList a)
type GetS     a = Get    '[JSON] (StripeList a)
type PostS    a = Post   '[JSON] (StripeList a)
type DeleteS  a = Delete '[JSON] (StripeList a)

type CustomerCreate =
     "v1"
  :> "customers"
  :> RBody CustomerCreateReq
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> PostS Customer

type CustomerRead =
     "v1"
  :> "customers"
  :> CapId CustomerId
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> GetS Customer

type CustomerUpdate =
     "v1"
  :> "customers"
  :> CapId CustomerId
  :> RBody CustomerUpdateReq
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> PostS Customer

type CustomerDestroy =
     "v1"
  :> "customers"
  :> CapId CustomerId
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> DeleteS Customer

type CustomerList =
     "v1"
  :> "customers"
  :> QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> GetS [Customer]


---- STRIPE ENDPOINT FUNCS ----

type RunnableStripeClient a = ClientM (StripeList a)

type PreRunnableStripeClient resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> RunnableStripeClient resp

type PaginatedPreRunnableStripeClient resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> PreRunnableStripeClient resp

type ListS           resp =              PaginatedPreRunnableStripeClient resp
type CreateS     req resp =       req -> PreRunnableStripeClient          resp
type UpdateS  id req resp = id -> req -> PreRunnableStripeClient          resp
type ReadS    id     resp = id ->        PreRunnableStripeClient          resp
type DestroyS id     resp = id ->        PreRunnableStripeClient          resp

createCustomer :: CreateS CustomerCreateReq Customer
readCustomer :: ReadS CustomerId Customer
updateCustomer :: UpdateS CustomerId CustomerUpdateReq Customer
destroyCustomer :: DestroyS CustomerId Customer
listCustomers :: ListS [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers = client (Proxy :: Proxy API)


-- stripe'' :: StripeSecretKey -> StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe'' = undefined
--
-- stripe' :: StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe' = stripe'' (StripeSecretKey "")
--
-- stripeList' :: StripeConnect -> [Pagination] -> PaginatedPreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripeList' = undefined
--
--
-- -- foo :: IO ()
-- -- foo = do
-- --   let id' = CustomerId ""
-- --       req = CustomerUpdateReq Nothing Nothing
-- --   eCustomer <- stripe' WithoutConnect $ updateCustomer id' req
-- --   print eCustomer
-- --   return ()



---- GENERAL STRIPE DATA TYPES ----

---- HEADER TYPES ----

newtype StripeSecretKey = StripeSecretKey String
-- newtype StripeAccountId = StripeAccountId String
data StripeVersion = StripeVersion'2017'08'15

instance ToHttpApiData StripeSecretKey where
  toUrlPiece (StripeSecretKey key) = T.pack . mconcat $ [ "Bearer ", key ]
instance ToHttpApiData StripeAccountId where
  toUrlPiece (StripeAccountId id') = T.pack id'
instance ToHttpApiData StripeVersion where
  toUrlPiece StripeVersion'2017'08'15 = "2017-08-15"


type StripeList a = Headers '[Header "Request-Id" String] (StripeJSON a)
data Stripe a = Stripe
  { stripeRequestId :: RequestId
  , stripeJson      :: StripeJSON a
  } deriving (Show, Generic)

data StripeJSON a = StripeJSON
  { stripeJsonObject  :: String
  , stripeJsonUrl     :: String
  , stripeJsonHasMore :: Bool
  , stripeJsonData    :: a
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 10 } ''StripeJSON)



---- CLIENT RUNNER ----

type StripeClient a =
     Maybe StripeVersion
  -> Maybe StripeSecretKey
  -> Maybe StripeAccountId
  -> Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> ClientM (StripeList a)

-- stripe'' :: StripeSecretKey -> StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe'' = undefined
--
-- stripe' :: StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe' = stripe'' (StripeSecretKey "")
--
-- stripeList' :: StripeConnect -> [Pagination] -> PaginatedPreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripeList' = undefined
--
-- -- type PreRunnableStripeClient resp =
-- --      Maybe StripeAccountId
-- --   -> Maybe StripeSecretKey
-- --   -> Maybe StripeVersion
-- --   -> RunnableStripeClient resp
-- --
-- -- type PaginatedPreRunnableStripeClient resp =
-- --      Maybe PaginationLimit
-- --   -> Maybe PaginationStartingAfter
-- --   -> Maybe PaginationEndingBefore
-- --   -> PreRunnableStripeClient resp
--
-- foo :: IO ()
-- foo = do
--   let id' = CustomerId ""
--       req = CustomerUpdateReq Nothing Nothing
--   eCustomer <- stripe' WithoutConnect $ updateCustomers id' req
--   print eCustomer
--   return ()


-- stripe' :: StripeSecretKey -> StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe' = undefined

sData :: Stripe a -> a
sData = stripeJsonData . stripeJson

stripeList' :: StripeSecretKey -> StripeConnect -> [Pagination] -> PaginatedPreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
stripeList' secretKey connect pagination clientM = stripe' secretKey connect clientM'
  where
    Pagination'{..} = buildPagination pagination
    clientM' = clientM paginateBy paginateStartingAfter paginateEndingBefore

stripe' :: StripeSecretKey -> StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
stripe' secretKey connect clientM = clientEnv >>= runClientM clientM' >>= buildStripe
  where
    clientM' =
      clientM
        (connectToMaybe connect)
        (Just secretKey)
        (Just StripeVersion'2017'08'15)
        -- StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url

    connectToMaybe s =
      case s of
        WithoutConnect  -> Nothing
        WithConnect id' -> Just id'

    buildStripe :: Either ServantError (StripeList a) -> IO (Either StripeFailure (Stripe a))
    buildStripe eResp =
      case eResp of
        Right resp -> return . Right . stripeFromResp $ resp
        -- Left  err  -> return . Left . Error $ err
        Left  err  -> return . Left . stripeError $ err
      where
        stripeFromResp :: StripeList a -> Stripe a
        stripeFromResp (Headers a hs) = Stripe (mReqId hs) a

        mReqId :: HList '[Header "Request-Id" String] -> String
        mReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
        mReqId _ = ""
