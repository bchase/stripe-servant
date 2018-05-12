{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe.Types where

import qualified Data.Text             as T
import           Data.Char             (toLower)
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

import           Stripe.Error
import           Stripe.Util        (fromJsonString)
import           Stripe.Data.Id     (BankAccountId (..), CardId (..),
                                     CustomerId (..), Token (..))



---- STRIPE CONNECT ----

data StripeConnect
  = WithoutConnect
  | WithConnect StripeAccountId

-- TODO check okay GeneralizedNewtypeDeriving -- ToHttpApiData -- WARNING: Do not derive this using DeriveAnyClass as the generated instance will loop indefinitely.
newtype ConnectApplicationFee = ConnectApplicationFee { feeInCents :: Int } deriving ( Show, Generic, ToHttpApiData )



-- TODO mv with `Stripe` below
newtype StripeSecretKey = StripeSecretKey { unStripeSecretKey :: T.Text }
newtype StripeAccountId = StripeAccountId { unStripeAccountId :: T.Text }
data    StripeVersion   = StripeVersion'2017'08'15
instance ToHttpApiData StripeSecretKey where
  toUrlPiece = mappend "Bearer " . unStripeSecretKey
instance ToHttpApiData StripeAccountId where
  toUrlPiece = unStripeAccountId
instance ToHttpApiData StripeVersion where
  toUrlPiece StripeVersion'2017'08'15 = "2017-08-15"
-- -- `Stripe` MONAD -- -- TODO mv Stripe? Stripe.Config?

data StripeConfig = StripeConfig
  { stripeVersion   :: StripeVersion
  , stripeSecretKey :: StripeSecretKey
  }

newtype Stripe a = Stripe { runStripe :: ReaderT StripeConfig ( ExceptT StripeFailure IO ) a }
  deriving ( Functor, Applicative, Monad, MonadReader StripeConfig, MonadError StripeFailure, MonadIO )

stripeIO :: StripeConfig -> Stripe a -> IO (Either StripeFailure a)
stripeIO cfg = runExceptT . flip runReaderT cfg . runStripe



-- -- SOURCES -- --

data Source
  = SCustomer     CustomerId
  | SCustomerCard CustomerId CardId
  | SToken        Token
  deriving (Show, Generic)

data SourceId
  = SourceToken Token
  | SourceCard  CardId
  | SourceBank  BankAccountId
  deriving (Show, Generic)

instance ToHttpApiData SourceId where
  toQueryParam (SourceToken tok)  = unToken tok
  toQueryParam (SourceCard  card) = unCardId card
  toQueryParam (SourceBank  ba)   = unBankAccountId ba



-- -- TIMES -- --

data StripeTime = StripeTime
  { getPOSIXTime :: Int
  , getUTCTime :: Time.UTCTime
  } deriving (Eq, Show, Generic)

instance J.FromJSON StripeTime where
  parseJSON (J.Number sci) =
    return $ StripeTime (int sci) (Time.posixSecondsToUTCTime . int $ sci)
    where
      int :: (Num a) => Scientific -> a
      int = fromInteger . coefficient
  parseJSON _ = mempty



-- -- METADATA -- --

newtype Metadata = Metadata { unMetadata :: HM.HashMap T.Text T.Text } deriving (Show, Generic)

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



-- -- GENERIC IDS -- --

newtype ResourceId = ResourceId T.Text deriving (Show, Generic, J.FromJSON)



-- -- PAGINATION -- --

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
  toUrlPiece (StartingAfter' (ResourceId id')) = T.pack . show $ id'
instance ToHttpApiData PaginationEndingBefore where
  toUrlPiece (EndingBefore' (ResourceId id')) = T.pack . show $ id'



-- -- INTERVALS -- --

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



-- -- CODES (e.g. COUNTRY, CURRENCY) -- --

data CountryCode
  = US
  | UnrecognizedCountryCode T.Text
  deriving (Show, Generic)

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
  deriving (Show, Generic)

instance J.FromJSON CurrencyCode where
  parseJSON (J.String "usd") = return USD
  parseJSON (J.String "jpy") = return JPY
  parseJSON (J.String str)   = return . UnrecognizedCurrencyCode $ str
  parseJSON _ = mempty

instance ToHttpApiData CurrencyCode where
  toQueryParam (UnrecognizedCurrencyCode code) = code
  toQueryParam code = T.pack . map toLower . show $ code



-- -- STATEMENT DESCRIPTORS -- --

newtype StatementDescriptor = StatementDescriptor { unStatementDescriptor :: T.Text } deriving (Show, Generic)

statementDescriptor :: String -> Maybe StatementDescriptor
statementDescriptor str =
  if length str <= 22
     then Just . StatementDescriptor . T.pack $ str
     else Nothing

instance ToHttpApiData StatementDescriptor where
  toQueryParam = unStatementDescriptor

instance J.FromJSON StatementDescriptor where
  parseJSON = fromJsonString StatementDescriptor



-- -- PRICES -- --

data Price = Price
  { stripeCurrency :: CurrencyCode
  , stripeAmount   :: Int
  }

instance Show Price where
  show (Price (UnrecognizedCurrencyCode curr) amount) =
    "$" ++ show amount ++ " (" ++ T.unpack curr ++ ")"
  show (Price USD amount) = "$" ++ show amount
  show (Price JPY amount) = "¥" ++ show amount
