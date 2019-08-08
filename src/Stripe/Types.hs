{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe.Types where

import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Char             (toLower)
import           Data.List             (intercalate)
import qualified Data.HashMap.Strict   as HM
import           Data.Scientific       (Scientific, coefficient)
import qualified Data.Time.Clock       as Time
import qualified Data.Time.Clock.POSIX as Time
import           Control.Monad.Reader  (MonadIO, MonadReader, ReaderT, runReaderT)
import           Control.Monad.Except  (MonadError, ExceptT, runExceptT)
import           GHC.Generics          (Generic)

import qualified Data.Aeson         as J
import           Servant.API
import           Web.Internal.FormUrlEncoded (unForm)
import qualified Web.Internal.FormUrlEncoded as F

import           Stripe.Util        (deriveFromJSON')
import           Stripe.Error
import           Stripe.Util        (fromJsonString)
import           Stripe.Data.Id     (AccountId (..), BankAccountId (..), CardId (..),
                                     CustomerId (..), CouponId (..), SubscriptionId (..),
                                     Token (..))



---- `Stripe` MONAD ---- TODO mv Stripe? Stripe.Config?

newtype Stripe a = Stripe { runStripe :: ReaderT Config ( ExceptT StripeFailure IO ) a }
  deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError StripeFailure, MonadIO )


data Config = Config
  { configVersion   :: Version
  , configSecretKey :: SecretKey
  }

newtype SecretKey = SecretKey { unSecretKey :: T.Text }

data Version = Version'2017'08'15


stripeIO :: Config -> Stripe a -> IO (Either StripeFailure a)
stripeIO cfg = runExceptT . flip runReaderT cfg . runStripe


instance ToHttpApiData SecretKey where
  toUrlPiece = mappend "Bearer " . unSecretKey

instance ToHttpApiData Version where
  toUrlPiece Version'2017'08'15 = "2017-08-15"



---- STRIPE CONNECT ----

data Connect
  = WithoutConnect
  | WithConnect AccountId

-- TODO check okay GeneralizedNewtypeDeriving ToHttpApiData
-- docs: "WARNING: Do not derive this using DeriveAnyClass as the generated instance will loop indefinitely."
newtype ConnectApplicationFee = ConnectApplicationFee { feeInCents :: Int } deriving ( Show, Generic, ToHttpApiData )



---- SOURCES ----

data Source
  = SCustomer     CustomerId
  | SCustomerCard CustomerId CardId
  | SToken        Token
  deriving ( Show, Generic )

data SourceId
  = SourceToken Token
  | SourceCard  CardId
  | SourceBank  BankAccountId
  deriving ( Show, Generic )

instance ToHttpApiData SourceId where
  toQueryParam (SourceToken tok)  = unToken tok
  toQueryParam (SourceCard  card) = unCardId card
  toQueryParam (SourceBank  ba)   = unBankAccountId ba



---- TIMES ----

data Time = Time
  { timePOSIX :: Int
  , timeUTC   :: Time.UTCTime
  } deriving ( Eq, Show, Generic )

time :: Time.UTCTime -> Time
time t = Time s t
  where s = floor $ Time.utcTimeToPOSIXSeconds t

instance ToHttpApiData Time where
  toQueryParam = T.pack . show . timePOSIX

instance J.FromJSON Time where
  parseJSON (J.Number sci) =
    return $ Time (int sci) (Time.posixSecondsToUTCTime . int $ sci)
    where
      int :: (Num a) => Scientific -> a
      int = fromInteger . coefficient
  parseJSON _ = mempty



---- METADATA ----

newtype Metadata = Metadata { unMetadata :: HM.HashMap T.Text T.Text } deriving ( Show, Generic )

metadata :: [(T.Text, T.Text)] -> Metadata
metadata = Metadata . HM.fromList

instance J.FromJSON Metadata where
  parseJSON (J.Object obj) = return . Metadata . HM.map valToText $ obj
    where
      valToText (J.String txt) = txt
      valToText _              = ""
  parseJSON _ = mempty

instance ToHttpApiData Metadata where
  toQueryParam _ = "" -- TODO ... handling via `addMetadataToForm`

addMetadataToForm :: Maybe Metadata -> F.Form -> F.Form -- TODO mv?
addMetadataToForm mMetadata form =
  let hm = HM.delete "metadata" . unForm $ form
   in case mMetadata of
        Just md -> F.Form . HM.union (keysToMetadataScopedKeys . unMetadata $ md) $ hm
        Nothing -> F.Form hm
  where
    keysToMetadataScopedKeys =
      HM.fromList . map (\(k, v) -> (T.concat ["metadata[", k, "]"], [v])) . HM.toList



---- GENERIC IDS ----

newtype ResourceId = ResourceId T.Text deriving ( Show, Generic, J.FromJSON )



---- PAGINATION ----

data PaginationOpt
  = By            Int
  | StartingAfter ResourceId
  | EndingBefore  ResourceId

newtype PaginationLimit         = By'            Int        deriving (Generic)
newtype PaginationStartingAfter = StartingAfter' ResourceId deriving (Generic)
newtype PaginationEndingBefore  = EndingBefore'  ResourceId deriving (Generic)

data PaginationOpts = PaginationOpts
  { paginateBy            :: Maybe PaginationLimit
  , paginateStartingAfter :: Maybe PaginationStartingAfter
  , paginateEndingBefore  :: Maybe PaginationEndingBefore
  }

instance ToHttpApiData PaginationLimit where
  toUrlPiece (By' num) = T.pack . show $ num
instance ToHttpApiData PaginationStartingAfter where
  toUrlPiece (StartingAfter' (ResourceId id')) = id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (EndingBefore' (ResourceId id')) = id'



---- INTERVALS ----

data Interval
  = Day
  | Week
  | Month
  | Year
  deriving ( Show, Generic )

instance J.FromJSON Interval where
  parseJSON (J.String "day")   = return Day
  parseJSON (J.String "week")  = return Week
  parseJSON (J.String "month") = return Month
  parseJSON (J.String "year")  = return Year
  parseJSON _ = mempty

instance ToHttpApiData Interval where
  toQueryParam = T.pack . map toLower . show



---- CODES (e.g. COUNTRY, CURRENCY) ----

data CountryCode
  = US
  | UnrecognizedCountryCode T.Text
  deriving ( Show, Generic )

instance J.FromJSON CountryCode where
  parseJSON (J.String "US") = return US
  parseJSON (J.String str)  = return . UnrecognizedCountryCode $ str
  parseJSON _ = mempty

instance ToHttpApiData CountryCode where
  toQueryParam (UnrecognizedCountryCode code) = code
  toQueryParam code = T.pack . show $ code


data CurrencyCode -- ISO4217
  = USD
  | JPY
  | UnrecognizedCurrencyCode T.Text
  deriving ( Show, Generic )

instance J.FromJSON CurrencyCode where
  parseJSON (J.String "usd") = return USD
  parseJSON (J.String "jpy") = return JPY
  parseJSON (J.String str)   = return . UnrecognizedCurrencyCode $ str
  parseJSON _ = mempty

instance ToHttpApiData CurrencyCode where
  toQueryParam (UnrecognizedCurrencyCode code) = code
  toQueryParam code = T.pack . map toLower . show $ code



---- STATEMENT DESCRIPTORS ----

newtype StatementDescriptor = StatementDescriptor { unStatementDescriptor :: T.Text } deriving ( Show, Generic )

statementDescriptor :: String -> Maybe StatementDescriptor
statementDescriptor str =
  if length str <= 22
     then Just . StatementDescriptor . T.pack $ str
     else Nothing

instance ToHttpApiData StatementDescriptor where
  toQueryParam = unStatementDescriptor

instance J.FromJSON StatementDescriptor where
  parseJSON = fromJsonString StatementDescriptor



---- PRICES ----

data Price = Price
  { priceCurrency :: CurrencyCode
  , priceAmount   :: Int
  }

instance Show Price where
  show (Price (UnrecognizedCurrencyCode curr) amt) =
    "$" ++ show amt ++ " (" ++ T.unpack curr ++ ")"
  show (Price JPY amt) = "¥" ++ commaSeparate amt -- TODO yen char not ASCII
  show (Price USD amt) =
    let (dollars, cents) = (amt `div` 100, amt `rem` 100)
     in "$" ++ commaSeparate dollars ++ "." ++ zeroPad2 (show cents)

zeroPad2 :: String -> String
zeroPad2 [  ] = ['0', '0']
zeroPad2 [ch] = ['0',  ch]
zeroPad2  cs  = cs

commaSeparate :: Int -> String
commaSeparate =
  intercalate "," . fmap reverse . reverse . groupsOf 3 . reverse . show
  where
    -- courtesy of https://stackoverflow.com/a/12876438 (with diagram)
    groupsOf :: Int -> [a] -> [[a]]
    groupsOf _ [] = []
    groupsOf n xs = take n xs : groupsOf n (drop n xs)



---- SUBSCRIPTIONS ----

data SubscriptionStatus
  = Trialing
  | Active
  | PastDue
  | Canceled
  | Unpaid
  deriving ( Show, Generic )

instance J.FromJSON SubscriptionStatus where
  parseJSON (J.String "trialing") = return Trialing
  parseJSON (J.String "active")   = return Active
  parseJSON (J.String "past_due") = return PastDue
  parseJSON (J.String "canceled") = return Canceled
  parseJSON (J.String "unpaid")   = return Unpaid
  parseJSON _                     = mempty

instance ToHttpApiData SubscriptionStatus where
  toQueryParam Trialing = "trialing"
  toQueryParam Active   = "active"
  toQueryParam PastDue  = "past_due"
  toQueryParam Canceled = "canceled"
  toQueryParam Unpaid   = "unpaid"



---- COUPONS & DISCOUNTS ---

data CouponDuration
  = Forever
  | Once
  | Repeating
  deriving ( Show, Generic )

instance J.FromJSON CouponDuration where
  parseJSON (J.String "forever")   = return Forever
  parseJSON (J.String "once")      = return Once
  parseJSON (J.String "repeating") = return Repeating
  parseJSON _                      = mempty

data Coupon = Coupon
  { couponId               :: CouponId
  , couponAmountOff        :: Int -- NOTE: positive
  , couponPercentOff       :: Int -- NOTE: positive
  , couponCurrency         :: CurrencyCode
  , couponValid            :: Bool

  , couponCreated          :: Time
  , couponRedeemBy         :: Time
  , couponDuration         :: CouponDuration
  , couponDurationInMonths :: Maybe Int -- NOTE: positive -- "Number of months the coupon applies. Null if coupon duration is `forever` or `once`."

  , couponMaxRedemptions   :: Int -- NOTE: positive
  , couponTimeRedeemed     :: Int -- NOTE: non-negative
  , couponLivemode         :: Bool
  , couponMetadata         :: Metadata
  } deriving ( Show, Generic )
$(deriveFromJSON' ''Coupon)

data Discount = Discount
  { discountCustomer     :: CustomerId
  , discountCoupon       :: Coupon
  , discountStart        :: Time
  , discountEnd          :: Time
  , discountSubscription :: SubscriptionId
  } deriving ( Show, Generic )
$(deriveFromJSON' ''Discount)



---- TIME FILTERING ----

data TimeFilter
  = TimeFilterAt    Time
  | TimeFilterGT    (GTFilter Time)
  | TimeFilterLT    (LTFilter Time)
  | TimeFilterRange (GTFilter Time) (LTFilter Time)
  deriving ( Show, Generic )

data GTFilter a
  = GT' a
  | GTE a
  deriving ( Show, Generic )

data LTFilter a
  = LT' a
  | LTE a
  deriving ( Show, Generic )


instance ToHttpApiData TimeFilter where
  toQueryParam _ = "" -- TODO ... handling via `timeFilterToFormFields` + `addToForm`
timeFilterToFormFields :: Text -> TimeFilter -> [(Text, [Text])]
timeFilterToFormFields key (TimeFilterAt    t    ) = [(key, [toQueryParam t])]
timeFilterToFormFields key (TimeFilterGT    gt   ) = let (k,v) = gt' gt in [(T.concat [key, "[", k, "]"], [toQueryParam v])]
timeFilterToFormFields key (TimeFilterLT    lt   ) = let (k,v) = lt' lt in [(T.concat [key, "[", k, "]"], [toQueryParam v])]
timeFilterToFormFields key (TimeFilterRange gt lt) =
  let f1 = let (k1,v1) = gt' gt in (T.concat [key, "[", k1, "]"], [toQueryParam v1])
      f2 = let (k2,v2) = lt' lt in (T.concat [key, "[", k2, "]"], [toQueryParam v2])
   in [ f1, f2 ]

gt' :: GTFilter Time -> (Text, Time)
gt' (GT' t) = ("gt",  t)
gt' (GTE t) = ("gte", t)
lt' :: LTFilter Time -> (Text, Time)
lt' (LT' t) = ("lt",  t)
lt' (LTE t) = ("lte", t)
