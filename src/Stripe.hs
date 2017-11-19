{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe where

import qualified Data.Text                 as T
import           Data.Proxy                (Proxy (Proxy))
import           GHC.Generics              (Generic)

-- import           GHC.TypeLits              (Symbol)
import           Data.Aeson                as J
import           Data.Aeson.Casing         (snakeCase)
import           Data.Aeson.TH             (Options (..), defaultOptions, deriveToJSON, deriveFromJSON)
import           Network.HTTP.Client.TLS   (newTlsManagerWith, tlsManagerSettings)
import           Servant.API
import           Servant.Client            (ClientM, ClientEnv (ClientEnv),
                                            Scheme (Https), BaseUrl (BaseUrl),
                                            ServantError (..), runClientM, client)

import           Stripe.Types              (ResourceId (ResourceId), RequestId)
import           Stripe.Error              (StripeFailure (..), stripeError)


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

data CustomerCreateReq = CustomerCreateReq
  { customerCreateEmail       :: Maybe String
  , customerCreateDescription :: Maybe String
  } deriving (Generic)
$(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerCreateReq)

data CustomerUpdateReq = CustomerUpdateReq
  { customerUpdateEmail       :: Maybe String
  , customerUpdateDescription :: Maybe String
  } deriving (Generic)
$(deriveToJSON defaultOptions { fieldLabelModifier = snakeCase . drop 14 } ''CustomerUpdateReq)

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

-- type StripeAPI = "v1" :> (ChargeList :<|> CustomerList)
-- type StripeAPI = ChargeList :<|> CustomerList :<|> CustomerUpdate
-- type StripeAPI = CustomerUpdate
-- type StripeAPI = CustomerList

type API =
  --      StripeList ("v1" :> "customers" :> GetS [Customer])
  -- :<|> LongCustomerUpdate (PostS Customer)
  -- :<|> StripeUpdate ("v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq :> PostS Customer)
  --     StripeUpdate ("v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq :> PostS Customer)
  CustomerList :<|> CustomerUpdate

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

type CustomerUpdate =
     "v1"
  :> "customers"
  :> CapId CustomerId
  :> RBody CustomerUpdateReq
  :> Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> PostS Customer

type StripeList   sub = StripeRequiredHeaders (StripeListHeaders sub)
type StripeUpdate sub = StripeRequiredHeaders sub
-- type API = StripeRequiredHeaders StripeEndpoints
-- type StripeEndpoints = StripeListHeaders StripeListEndpoints
-- type StripeListEndpoints =
--        "v1" :> "customers" :> GetS [Customer]
--   -- :<|> "v1" :> "customers" :> ReqBody '[JSON] CustomerUpdateReq :> PostS Customer


type GetS  a = Get  '[JSON] (StripeResp a)
type PostS a = Post '[JSON] (StripeResp a)
type CapId t = Capture "id" t
type RBody t = ReqBody '[JSON] t

type LongCustomerUpdate verb =
     "v1" :> "customers" :> CapId CustomerId :> RBody CustomerUpdateReq
  :> Header     "Stripe-Version" StripeVersion
  :> Header     "Authorization"  StripeSecretKey
  :> Header     "Stripe-Account" StripeAccountId
  :> verb

-- type StripeShow sub =

type StripeRequiredHeaders sub =
     Header     "Stripe-Version" StripeVersion
  :> Header     "Authorization"  StripeSecretKey
  :> Header     "Stripe-Account" StripeAccountId
  :> sub
type StripeListHeaders sub =
     QueryParam "limit"          PaginationLimit
  :> QueryParam "starting_after" PaginationStartingAfter
  :> QueryParam "ending_before"  PaginationEndingBefore
  :> sub

-- type StripeAPI' = StripeAPI'' StripeAPI'''
-- type StripeAPI'' api =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> QueryParam "limit"          PaginationLimit
--   :> QueryParam "starting_after" PaginationStartingAfter
--   :> QueryParam "ending_before"  PaginationEndingBefore
--   :> api
--
-- type StripeAPI''' = CustomerList'
-- type CustomerList' = "v1" :> "customers" :> Get '[JSON] (StripeResp [Customer])

-- -- type StripeRoute route =
-- --      Header     "Stripe-Version" StripeVersion
-- --   :> Header     "Authorization"  StripeSecretKey
-- --   :> Header     "Stripe-Account" StripeAccountId
-- --   :> QueryParam "limit"          PaginationLimit
-- --   :> QueryParam "starting_after" PaginationStartingAfter
-- --   :> QueryParam "ending_before"  PaginationEndingBefore
-- --   :> route
--
-- -- type ChargeList   = StripeRoute ("charges"   :> Get '[JSON] (StripeResp [Charge]))
-- -- type CustomerList = StripeRoute ("customers" :> Get '[JSON] (StripeResp [Customer]))

-- type StripeList sub =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> QueryParam "limit"          PaginationLimit
--   :> QueryParam "starting_after" PaginationStartingAfter
--   :> QueryParam "ending_before"  PaginationEndingBefore
--   :> sub
--
-- data StripeList' sub a =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> QueryParam "limit"          PaginationLimit
--   :> QueryParam "starting_after" PaginationStartingAfter
--   :> QueryParam "ending_before"  PaginationEndingBefore
--   :> sub
--   :> Get '[JSON] (StripeResp a)
-- foo :: (->) String (Int -> [Bool])
-- foo s i = [ length s == i ]
--
-- type StripeList' (((:>) ("v1" :: Symbol) sub) :: (:>) =
--   --    Header     "Stripe-Version" StripeVersion
--   -- :> Header     "Authorization"  StripeSecretKey
--   -- :> Header     "Stripe-Account" StripeAccountId
--   -- :> QueryParam "limit"          PaginationLimit
--   -- :> QueryParam "starting_after" PaginationStartingAfter
--   -- :> QueryParam "ending_before"  PaginationEndingBefore
--   "v1" :> sub

-- type StripeCreate sub req a =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> sub
--   :> ReqBody '[JSON] req
--   :> Post '[JSON] (StripeResp a)
--
-- type StripeShow sub a =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> sub
--   :> Get '[JSON] (StripeResp a)
--
-- type StripeUpdate' sub req a =
--      sub
--   :> Header "Stripe-Version" StripeVersion
--   :> Header "Authorization"  StripeSecretKey
--   :> Header "Stripe-Account" StripeAccountId
--   :> ReqBody '[JSON] req
--   :> Post '[JSON] (StripeResp a)
-- type StripeUpdate sub =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> sub
-- type StripeUpdate sub req a =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> sub
--   :> ReqBody '[JSON] req
--   :> Post '[JSON] (StripeResp a)

-- type StripeDelete sub a =
--      Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> sub
--   :> Delete '[JSON] (StripeResp a)

-- type ChargeList = StripeList ("v1" :> "charges" :> GetS [Charge])
--
-- type CustomerList   = StripeList   ("v1" :> "customers" :> GetS [Customer])
-- -- type CustomerCreate' = StripeCreate ("v1" :> "customers") CustomerCreateReq Customer
-- -- type CustomerShow'   = StripeShow   ("v1" :> "customers" :> Capture "customer-show-id" CustomerId) Customer
-- -- type CustomerDelete' = StripeDelete ("v1" :> "customers" :> Capture "customer-delete-id" CustomerId) Customer
-- type CustomerUpdate = StripeUpdate ("v1" :> "customers" :> Capture "customer-update-id" CustomerId :> ReqBody '[JSON] CustomerUpdateReq :> PostS Customer)
-- type CustomerUpdate' = StripeUpdate' ("v1" :> "customers" :> Capture "customer-update-id" CustomerId) CustomerUpdateReq Customer

-- type StripeGet route a =
--      route
--   :> Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> QueryParam "limit"          PaginationLimit
--   :> QueryParam "starting_after" PaginationStartingAfter
--   :> QueryParam "ending_before"  PaginationEndingBefore
--   :> Get '[JSON] (StripeResp a)
--
-- type StripePost route a =
--      route
--   :> Header     "Stripe-Version" StripeVersion
--   :> Header     "Authorization"  StripeSecretKey
--   :> Header     "Stripe-Account" StripeAccountId
--   :> Post '[JSON] (StripeResp a)
--
-- newtype CustomerId = CustomerId String deriving (Generic)
-- data CustomerUpdateReq = CustomerUpdateReq
--   { customerUpdateDescription :: Maybe String
--   } deriving (Generic)
-- type CustomerUpdateInput =
--      "customers"
--   :> Capture "customer-id" CustomerId
--   :> ReqBody '[JSON] CustomerUpdateReq
-- type CustomerUpdateRoute = StripeGet CustomerUpdateInput Customer

-- type CustomerUpdate =
--      "customers"
--   :> Capture "customer-id" CustomerId
--   :> ReqBody '[JSON] CustomerUpdateReq
--   :> StripeRoute'
--   :> Post '[JSON] (StripeResp [Customer])


---- STRIPE ENDPOINT FUNCS ----

-- type StripeClient' a = (->) (Maybe StripeSecretKey) (ClientM (StripeResp a))

type StripeClient a =
     Maybe StripeVersion
  -> Maybe StripeSecretKey
  -> Maybe StripeAccountId
  -> Maybe PaginationLimit
  -> Maybe PaginationStartingAfter
  -> Maybe PaginationEndingBefore
  -> ClientM (StripeResp a)

type RunnableStripeClient a = ClientM (StripeResp a)

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

type ListS          resp =              PaginatedPreRunnableStripeClient resp
type CreateS    req resp =       req -> PreRunnableStripeClient          resp
type UpdateS id req resp = id -> req -> PreRunnableStripeClient          resp
type ReadS   id     resp = id ->        PreRunnableStripeClient          resp
type DeleteS id     resp = id ->        PreRunnableStripeClient          resp

-- -- getCharges :: StripeClient [Charge]
-- getCharges :: Maybe StripeVersion
--            -> Maybe StripeSecretKey
--            -> Maybe StripeAccountId
--            -> Maybe PaginationLimit
--            -> Maybe PaginationStartingAfter
--            -> Maybe PaginationEndingBefore
--            -> ClientM (StripeResp [Charge])
-- getCustomers :: Maybe StripeVersion
--              -> Maybe StripeSecretKey
--              -> Maybe StripeAccountId
--              -> Maybe PaginationLimit
--              -> Maybe PaginationStartingAfter
--              -> Maybe PaginationEndingBefore
--              -> ClientM (StripeResp [Customer])
-- updateCustomers :: CustomerUpdateReq
--                 -> Maybe StripeVersion
--                 -> Maybe StripeSecretKey
--                 -> Maybe StripeAccountId
--                 -> ClientM (StripeResp Customer)
--
-- updateCustomers :: Maybe StripeVersion
--                 -> Maybe StripeSecretKey
--                 -> Maybe StripeAccountId
--                 -> CustomerId
--                 -> CustomerUpdateReq
--                 -> ClientM (StripeResp Customer)
getCustomers :: ListS [Customer]
updateCustomers :: UpdateS CustomerId CustomerUpdateReq Customer
-- updateCustomers :: Maybe StripeVersion
--                 -> Maybe StripeSecretKey
--                 -> Maybe StripeAccountId
--                 -> CustomerId
--                 -> CustomerUpdateReq
--                 -> ClientM (StripeResp Customer)
--
-- getCharges :<|> getCustomers :<|> updateCustomers = client (Proxy :: Proxy StripeAPI)
-- updateCustomers = client (Proxy :: Proxy StripeAPI)
-- getCustomers = client (Proxy :: Proxy API)
getCustomers :<|> updateCustomers = client (Proxy :: Proxy API)
-- updateCustomers = client (Proxy :: Proxy API)

-- stripeU :: UpdateS id' req resp -> StripeConnect -> StripeSecretKey -> RunnableStripeClient resp
-- stripeU = undefined
-- stripe' :: RunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
-- stripe' = undefined

stripe'' :: StripeSecretKey -> StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
stripe'' = undefined

stripe' :: StripeConnect -> PreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
stripe' = stripe'' (StripeSecretKey "")

stripeList' :: StripeConnect -> [Pagination] -> PaginatedPreRunnableStripeClient resp -> IO (Either StripeFailure (Stripe resp))
stripeList' = undefined


foo :: IO ()
foo = do
  let id' = CustomerId ""
      req = CustomerUpdateReq Nothing Nothing
  eCustomer <- stripe' WithoutConnect $ updateCustomers id' req
  print eCustomer
  return ()



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


type StripeResp a = Headers '[Header "Request-Id" String] (StripeJSON a)
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


-- stripe :: StripeConnect -> [Pagination] -> StripeClient a -> IO (Either Error (Stripe a))
stripe :: StripeConnect -> [Pagination] -> StripeClient a -> IO (Either StripeFailure (Stripe a))
stripe connect pagination clientM = clientEnv >>= runClientM clientM' >>= buildStripe
  where
    clientM' =
      clientM
        (Just StripeVersion'2017'08'15)
        (Just . StripeSecretKey $ "sk_test_BQokikJOvBiI2HlWgH4olfQ2")
        (connectToMaybe connect)
        paginateBy
        paginateStartingAfter
        paginateEndingBefore
      where Pagination'{..} = buildPagination pagination

    clientEnv = do
      manager <- newTlsManagerWith tlsManagerSettings
      let url = BaseUrl Https "api.stripe.com" 443 ""
      return $ ClientEnv manager url

    connectToMaybe s =
      case s of
        WithoutConnect  -> Nothing
        WithConnect id' -> Just id'

    -- buildStripe :: Either ServantError (StripeResp a) -> IO (Either Error (Stripe a))
    buildStripe :: Either ServantError (StripeResp a) -> IO (Either StripeFailure (Stripe a))
    buildStripe eResp =
      case eResp of
        Right resp -> return . Right . stripeFromResp $ resp
        -- Left  err  -> return . Left . Error $ err
        Left  err  -> return . Left . stripeError $ err
      where
        stripeFromResp :: StripeResp a -> Stripe a
        stripeFromResp (Headers a hs) = Stripe (mReqId hs) a

        mReqId :: HList '[Header "Request-Id" String] -> String
        mReqId ((Header id' :: Header "Request-Id" String) `HCons` HNil) = id'
        mReqId _ = ""


-- stripeList :: StripeConnect
--            -> [Pagination]
--            -> StripeListClient a
--            -> IO (Either StripeFailure (Stripe a))
-- stripeList = undefined
--
-- stripeUpdate :: (ResId id')
--              => StripeConnect
--              -> id'
--              -> ReqFor id'
--              -> ClientM (StripeResp a)
--              -> IO (Either StripeFailure (Stripe a))
-- stripeUpdate = undefined

-- do
--   let cusId = CustomerId "cus_1234abcd"
--       req   = customerUpdateReq { customerUpdateDescription = "new description"}
--   eCus <- stripeUpdate updateCustomer WithoutConnect cusId req
--
-- do
--   let cusId = CustomerId "cus_1234abcd"
--       req   = customerUpdateReq { customerUpdateDescription = "new description"}
--   eCus <- stripe $ updateCustomer Nothing cusId req

-- -- -- GENERAL PARAMS FIRST -- --
-- -- LIST
-- stripeL WithoutConnect [] listCharges
-- -- LIST (NESTED)
-- stripeLN (WithConnect (StripeAccountId "acct_1234abcd")) [PaginateBy 10] listCards (CustomerId "cus_1234abcd")

-- class Updates req
--
-- instance Updates UpdateReq where
--
-- -- data UpdateReq = UpdateReq
-- -- data UpdateReqBody = UpdateReq { foo :: String }
-- type RunnableStripeClient a = ClientM (StripeResp a)
-- type StripeListClient a =
--      Maybe StripeVersion
--   -> Maybe StripeSecretKey
--   -> Maybe StripeAccountId
--   -> Maybe PaginationLimit
--   -> Maybe PaginationStartingAfter
--   -> Maybe PaginationEndingBefore
--   -> RunnableStripeClient a
-- type StripeUpdateClient a =
--      Maybe StripeVersion
--   -> Maybe StripeSecretKey
--   -> Maybe StripeAccountId
--   -> id'
--   ->
--   -> RunnableStripeClient a
-- stripeL :: StripeListClient a -> [Pagination] -> RunnableStripeClient a
-- stripeL = undefined
-- -- -- GENERAL PARAMS LAST -- --
-- -- LIST
-- stripeL WithoutConnect [] listCharges
-- -- LIST (NESTED)
-- stripeLN (WithConnect (StripeAccountId "acct_1234abcd")) [PaginateBy 10] listCards (CustomerId "cus_1234abcd")

-- -- CREATE
-- -- READ
-- -- UPDATE
-- -- DELETE
-- -- LIST

-- updateCustomers' :: Maybe StripeVersion
--                  -> Maybe StripeSecretKey
--                  -> Maybe StripeAccountId
--                  -> CustomerId
--                  -> CustomerUpdateReq
--                  -> RunnableStripeClient Customer
-- updateCustomers' = undefined
updateCustomers' :: CustomerId
                 -> CustomerUpdateReq
                 -> Maybe StripeAccountId
                 -> Maybe StripeSecretKey
                 -> Maybe StripeVersion
                 -> RunnableStripeClient Customer
updateCustomers' = undefined
                 -- -> Maybe StripeAccountId
                 -- -> Maybe StripeSecretKey
                 -- -> Maybe StripeVersion
                 -- -> RunnableStripeClient Customer

-- {-# LANGUAGE ExistentialQuantification  #-}
--
-- class UpdateRequest req where
--   data Id req
--
-- data Update req = UpdateRequest req => Update
--   { updateResourceId :: Id req
--   , updateRequest    :: req
--   }



-- type CustomerUpdate = CustomerId -> CustomerUpdateReq -> PreRunnableStripeClient Customer
-- -- stripeU ::
-- updateCustomers'' :: CustomerUpdate
-- updateCustomers'' = updateCustomers'
--
-- stripe' :: PreRunnableStripeClient a -> StripeConnect -> StripeSecretKey -> RunnableStripeClient a
-- stripe' preClient connect key =
--   preClient (connectToMaybe connect) (Just key) (Just StripeVersion'2017'08'15)
--   where
--     connectToMaybe s =
--       case s of
--         WithoutConnect  -> Nothing
--         WithConnect id' -> Just id'
--
-- -- do :: (MonadReader Config m, MonadIO m) => m ()
-- --   secretKey <- asks stripeSecretKey
-- --   customer <- liftIO . stripe WithoutConnect secretKey (updateCustomers' customerId updateReq)
