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
import           Data.Either                 (either)
import           GHC.Generics                (Generic)

-- import           GHC.TypeLits                (Symbol)
import           Data.Aeson                  as J
import           Data.Aeson.Casing           (snakeCase)
import           Data.Aeson.TH               (Options (..), defaultOptions, deriveFromJSON)
import           Network.HTTP.Client.TLS     (newTlsManagerWith, tlsManagerSettings)
import           Web.Internal.FormUrlEncoded as F
import           Servant.API
import           Servant.Client              (ClientM, ClientEnv (ClientEnv),
                                              Scheme (Https), BaseUrl (BaseUrl),
                                              runClientM, client)

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
--   - nested CRUD
--   - hardcode query param?
--   e.g.
--   GET https://api.stripe.com/v1/customers/{CUSTOMER_ID}/sources?object=bank_account


newtype StripeAccountId = StripeAccountId String

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

---- PAGINATION ----

---- PUBLIC PAGINATION ----

data PaginationOpt
  = PaginateBy Int -- Nat
  | PaginateStartingAfter ResourceId
  | PaginateEndingBefore ResourceId

---- PRIVATE PAGINATION ----

newtype PaginationLimit         = PaginateBy'            Int        deriving (Generic)
newtype PaginationStartingAfter = PaginateStartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = PaginateEndingBefore'  ResourceId deriving (Generic)

data PaginationOpts = PaginationOpts
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

buildPagination :: [PaginationOpt] -> PaginationOpts
buildPagination = foldl updatePagination emptyPagination
  where
    emptyPagination = PaginationOpts Nothing Nothing Nothing
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

newtype ChargeId   = ChargeId   { unChargeId   :: T.Text } deriving (Eq, Show, Generic)
newtype CustomerId = CustomerId { unCustomerId :: T.Text } deriving (Eq, Show, Generic)

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
  { customerCreateSource      :: Token
  , customerCreateEmail       :: Maybe String
  , customerCreateDescription :: Maybe String
  } deriving (Generic)
-- $(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerCreateReq)
instance F.ToForm CustomerCreateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
minCustomerCreateReq :: Token -> CustomerCreateReq
minCustomerCreateReq token = CustomerCreateReq token Nothing Nothing

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving (Generic)
-- $(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerUpdateReq)
instance F.ToForm CustomerUpdateReq where
  toForm = F.genericToForm $ F.defaultFormOptions { F.fieldLabelModifier = snakeCase . drop 14 }
emptyCustomerUpdateReq :: CustomerUpdateReq
emptyCustomerUpdateReq = CustomerUpdateReq Nothing Nothing

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

type GetListS a = Get    '[JSON] (StripeListResp   a)
type GetShowS a = Get    '[JSON] (StripeResp       a)
type PostS    a = Post   '[JSON] (StripeResp       a)
type DeleteS  a = Delete '[JSON] (StripeDeleteResp a)

type StripeHeaders resp =
     Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> resp

type StripePaginationQueryParams resp =
     QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> resp

type CustomerCreate  = "v1" :> "customers" :> RBody CustomerCreateReq :> StripeHeaders (PostS Customer)
type CustomerRead    = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (GetShowS Customer)
type CustomerUpdate  = "v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq :> StripeHeaders (PostS Customer)
type CustomerDestroy = "v1" :> "customers" :> CapId CustomerId :> StripeHeaders (DeleteS CustomerId)
type CustomerList    = "v1" :> "customers" :> StripePaginationQueryParams (StripeHeaders (GetListS [Customer]))


---- STRIPE ENDPOINT FUNCS ----

type RunnableStripeClient a = ClientM a

type PreRunnableStripeClient resp =
     Maybe StripeAccountId
  -> Maybe StripeSecretKey
  -> Maybe StripeVersion
  -> RunnableStripeClient resp

type PreRunnableStripeListClient resp =
     Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> PreRunnableStripeClient (StripeListResp resp)

type PreRunnableStripeDeleteClient id =
  PreRunnableStripeClient (StripeDeleteResp id)

type PreRunnableStripeScalarClient resp =
  PreRunnableStripeClient (StripeResp resp)

type ListS           resp =              PreRunnableStripeListClient   resp
type CreateS     req resp =       req -> PreRunnableStripeScalarClient resp
type UpdateS  id req resp = id -> req -> PreRunnableStripeScalarClient resp
type ReadS    id     resp = id ->        PreRunnableStripeScalarClient resp
type DestroyS id          = id ->        PreRunnableStripeDeleteClient   id

createCustomer :: CreateS CustomerCreateReq Customer
readCustomer :: ReadS CustomerId Customer
updateCustomer :: UpdateS CustomerId CustomerUpdateReq Customer
destroyCustomer :: DestroyS CustomerId
listCustomers :: ListS [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers = client (Proxy :: Proxy API)


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


type StripeResp       a  = Headers '[Header "Request-Id" String]                    a
type StripeListResp   a  = Headers '[Header "Request-Id" String] (StripeListJSON    a)
type StripeDeleteResp id = Headers '[Header "Request-Id" String] (StripeDeleteJSON id)

data Stripe a = Stripe
  { stripeRequestId :: RequestId
  , stripeData      :: a
  } deriving (Show, Generic)

data StripeList a = StripeList
  { stripeListRequestId :: RequestId
  , stripeListHasMore   :: Bool
  , stripeListData      :: a
  } deriving (Show, Generic)

data StripeDelete id = StripeDelete
  { stripeDeleteRequestId :: RequestId
  , stripeDeleteId        :: id
  , stripeDeleteDeleted   :: Bool
  } deriving (Show, Generic)

data StripeListJSON a = StripeListJSON
  { stripeListJsonObject  :: String
  , stripeListJsonUrl     :: String
  , stripeListJsonHasMore :: Bool
  , stripeListJsonData    :: a
  } deriving (Show, Generic)

data StripeDeleteJSON id = StripeDeleteJSON
  { stripeDeleteJsonId      :: id
  , stripeDeleteJsonDeleted :: Bool
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''StripeListJSON)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop 16 } ''StripeDeleteJSON)



---- CLIENT RUNNER ----

stripeList' :: StripeSecretKey
            -> StripeConnect
            -> [PaginationOpt]
            -> PreRunnableStripeListClient resp
            -> IO (Either StripeFailure (StripeList resp))
stripeList' secretKey connect pagination clientM =
  clientEnv >>= runClientM clientM' >>= return . either (Left . stripeError) (Right . stripeListFromResp)
  where
    PaginationOpts{..} = buildPagination pagination

    clientM' =
      clientM
        paginateBy
        paginateStartingAfter
        paginateEndingBefore
        (connectToMaybe connect)
        (Just secretKey)
        (Just version)

    stripeListFromResp :: StripeListResp a -> StripeList a
    stripeListFromResp (Headers StripeListJSON{stripeListJsonHasMore, stripeListJsonData} hs) =
      StripeList (getReqId hs) stripeListJsonHasMore stripeListJsonData

stripeDelete' :: StripeSecretKey
              -> StripeConnect
              -> PreRunnableStripeDeleteClient id
              -> IO (Either StripeFailure (StripeDelete id))
stripeDelete' secretKey connect clientM =
  clientEnv >>= runClientM clientM' >>= return . either (Left . stripeError) (Right . stripeDeleteFromResp)
  where
    clientM' =
      clientM
        (connectToMaybe connect)
        (Just secretKey)
        (Just version)

    stripeDeleteFromResp :: StripeDeleteResp a -> StripeDelete a
    stripeDeleteFromResp (Headers StripeDeleteJSON{stripeDeleteJsonId, stripeDeleteJsonDeleted} hs) =
      StripeDelete (getReqId hs) stripeDeleteJsonId stripeDeleteJsonDeleted

stripe' :: StripeSecretKey
        -> StripeConnect
        -> PreRunnableStripeScalarClient resp
        -> IO (Either StripeFailure (Stripe resp))
stripe' secretKey connect clientM =
  clientEnv >>= runClientM clientM' >>= return . either (Left . stripeError) (Right . stripeFromResp)
  where
    clientM' =
      clientM
        (connectToMaybe connect)
        (Just secretKey)
        (Just version)

    stripeFromResp :: StripeResp a -> Stripe a
    stripeFromResp (Headers a hs) = Stripe (getReqId hs) a

---- CLIENT RUNNER HELPERS ----

version :: StripeVersion
version = StripeVersion'2017'08'15

getReqId :: HList '[Header "Request-Id" String] -> String
getReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
getReqId _ = ""

connectToMaybe :: StripeConnect -> Maybe StripeAccountId
connectToMaybe s =
  case s of
    WithoutConnect  -> Nothing
    WithConnect id' -> Just id'

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newTlsManagerWith tlsManagerSettings
  let url = BaseUrl Https "api.stripe.com" 443 ""
  return $ ClientEnv manager url
