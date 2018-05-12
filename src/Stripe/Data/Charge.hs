{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stripe.Data.Charge where

import qualified Data.Text      as T
import           GHC.Generics   (Generic)

import           Data.Aeson     as J

import           Stripe.Util    (deriveFromJSON')
import           Stripe.Error   (StripeErrorCode)
import           Stripe.Data.Id (ChargeId, CustomerId, AccountId, InvoiceId)
import           Stripe.Types   (Time (..), CurrencyCode (..), Price(..),
                                 Metadata (..), StatementDescriptor (..))



data Charge = Charge
  { chargeId                  :: ChargeId
  , chargeAmount              :: Int
  , chargeAmountRefunded      :: Int
  , chargeCaptured            :: Bool
  , chargeCreated             :: Time
  , chargeCurrency            :: CurrencyCode
  , chargeCustomer            :: Maybe CustomerId
  , chargeDescription         :: Maybe String
  , chargeDestination         :: Maybe AccountId
  , chargeFailureCode         :: Maybe StripeErrorCode
  , chargeFailureMessage      :: Maybe String
  , chargeInvoice             :: Maybe InvoiceId
  , chargeLivemode            :: Bool
  , chargeMetadata            :: Metadata
  , chargeOnBehalfOf          :: Maybe AccountId
  , chargeOutcome             :: ChargeOutcome
  , chargePaid                :: Bool
  , chargeReceiptEmail        :: Maybe String
  , chargeReceiptNumber       :: Maybe String
  , chargeRefunded            :: Bool
  , chargeStatementDescriptor :: Maybe StatementDescriptor
  , chargeStatus              :: ChargeStatus
  } deriving ( Show, Generic )



---- RELATED TYPES ----

data ChargeStatus
  = Succeeded
  | Pending
  | Failed
  deriving ( Show, Generic )


data ChargeOutcome = ChargeOutcome
  { chargeOutcomeNetworkStatus :: ChargeOutcomeNetworkStatus
  , chargeOutcomeReason        :: Maybe ChargeOutcomeReason
  , chargeOutcomeRiskLevel     :: ChargeOutcomeRiskLevel
  , chargeOutcomeSellerMessage :: String
  , chargeOutcomeType          :: ChargeOutcomeType
  } deriving ( Show, Generic )

data ChargeOutcomeNetworkStatus
  = ApprovedByNetwork
  | DeclinedByNetwork
  | NotSentToNetwork
  | ReversedAfterApproval
  | UnrecognizedChargeOutcomeNetworkStatus T.Text
  deriving ( Show, Generic )

data ChargeOutcomeRiskLevel
  = Normal
  | Elevated
  | Highest
  | NotAssessed
  | UnknownRiskLevel
  deriving ( Show, Generic )

data ChargeOutcomeType
  = Authorized
  | ManualReview
  | IssuerDeclined
  | Blocked
  | Invalid
  deriving ( Show, Generic )

data ChargeOutcomeReason
  = HighestRiskLevel
  | ElevatedRiskLevel
  | Rule
  | OtherReason T.Text
  deriving ( Show, Generic )



---- HELPERS ----

chargePrice :: Charge -> Price
chargePrice Charge{..} = Price chargeCurrency chargeAmount



---- FromJSON INSTANCES ----

$(deriveFromJSON' ''Charge)

$(deriveFromJSON' ''ChargeOutcome)

instance J.FromJSON ChargeStatus where
  parseJSON (J.String "succeeded") = return Succeeded
  parseJSON (J.String "pending")   = return Pending
  parseJSON (J.String "failed")    = return Failed
  parseJSON _ = mempty

instance J.FromJSON ChargeOutcomeNetworkStatus where
  parseJSON (J.String "approved_by_network")     = return ApprovedByNetwork
  parseJSON (J.String "declined_by_network")     = return DeclinedByNetwork
  parseJSON (J.String "not_sent_to_network")     = return NotSentToNetwork
  parseJSON (J.String "reversed_after_approval") = return ReversedAfterApproval
  parseJSON (J.String str)                       = return $ UnrecognizedChargeOutcomeNetworkStatus str
  parseJSON _                                    = return $ UnrecognizedChargeOutcomeNetworkStatus ""

instance J.FromJSON ChargeOutcomeRiskLevel where
  parseJSON (J.String "normal")       = return Normal
  parseJSON (J.String "elevated")     = return Elevated
  parseJSON (J.String "highest")      = return Highest
  parseJSON (J.String "not_assessed") = return NotAssessed
  parseJSON (J.String "unknown")      = return UnknownRiskLevel
  parseJSON _ = mempty

instance J.FromJSON ChargeOutcomeType where
  parseJSON (J.String "authorized")      = return Authorized
  parseJSON (J.String "manual_review")   = return ManualReview
  parseJSON (J.String "issuer_declined") = return IssuerDeclined
  parseJSON (J.String "blocked")         = return Blocked
  parseJSON (J.String "invalid")         = return Invalid
  parseJSON _ = mempty

instance J.FromJSON ChargeOutcomeReason where
  parseJSON (J.String "highest_risk_level")  = return HighestRiskLevel
  parseJSON (J.String "elevated_risk_level") = return ElevatedRiskLevel
  parseJSON (J.String "rule")                = return Rule
  parseJSON _ = mempty



-- -- TODO additional `Charge` properties
-- , chargeRefunds :: {
--     Object :: List,
--     Data :: [...],
--     HasMore :: False,
--     TotalCount :: 0,
--     Url :: /v1/Charges/Ch1BS9Tl2EZvKYlo2CRwnFr7F6/Refunds
--   },
-- , chargeSource :: {...} -- Source = Card | BA
-- , chargeApplication :: Maybe    -- NOTE expandable "ID of the Connect application that created to charge"
-- , chargeApplicationFee :: Maybe -- NOTE Connect
-- , chargeBalanceTransaction ::   -- NOTE expandable "ID of the balance transaction that describes the impact of this charge on your account balance (not including refunds or disputes)."
-- , chargeDispute :: Null,
-- , chargeFraudDetails :: { },
--
-- -- Relay, Radar, etc.
-- , chargeOrder :: Null,
-- , chargeReview :: Null,
-- , chargeShipping :: Null, -- expandable
-- , chargeSourceTransfer :: Null,
-- , chargeTransferGroup :: Null
