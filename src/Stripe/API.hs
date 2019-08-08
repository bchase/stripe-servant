{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Stripe.API where

import           Prelude    hiding (Read)
import           Data.Proxy (Proxy (Proxy))

import           Servant.API
import           Servant.Client (client)

import           Stripe.Data.Id
import           Stripe.Data.BankAccount
import           Stripe.Data.Card
import           Stripe.Data.Charge
import           Stripe.Data.Customer
import           Stripe.Data.Plan
import           Stripe.Data.Subscription
import           Stripe.API.Request.BankAccount
import           Stripe.API.Request.Card
import           Stripe.API.Request.Charge
import           Stripe.API.Request.Customer
import           Stripe.API.Request.Plan
import           Stripe.API.Request.Subscription
import           Stripe.API.HTTP (Id, Body, GetL, Get', Post', Delete', Delete'',
                                  List, List', Create, Read, Update, Destroy, Destroy')



type ChargeAPI
     = ChargeCreate
  :<|> ChargeRead
  :<|> ChargeUpdate
  :<|> ChargeList
  :<|> ChargeCapture

type CustomerAPI
     = CustomerCreate
  :<|> CustomerRead
  :<|> CustomerUpdate
  :<|> CustomerDestroy
  :<|> CustomerList

type CustomerCardAPI
     = CustomerCardCreate
  :<|> CustomerCardRead
  :<|> CustomerCardUpdate
  :<|> CustomerCardDestroy
  :<|> CustomerCardList

type CustomerBankAccountAPI
     = CustomerBankAccountCreate
  :<|> CustomerBankAccountRead
  :<|> CustomerBankAccountUpdate
  :<|> CustomerBankAccountDestroy
  :<|> CustomerBankAccountList
  :<|>  CustomerBankAccountVerify

type PlanAPI
     = PlanCreate
  :<|> PlanRead
  :<|> PlanUpdate
  :<|> PlanDestroy
  :<|> PlanList

type SubscriptionAPI
     = SubscriptionCreate
  :<|> SubscriptionRead
  -- :<|> SubscriptionUpdate
  :<|> SubscriptionDestroy
  :<|> SubscriptionList
  :<|> SubscriptionList'


type ChargeCreate  = "v1" :> "charges" :> Body ChargeCreateReq :> Post' Charge
type ChargeRead    = "v1" :> "charges" :> Id ChargeId :> Get' Charge
type ChargeUpdate  = "v1" :> "charges" :> Id ChargeId :> Body ChargeUpdateReq :> Post' Charge
type ChargeList    = "v1" :> "charges" :> GetL [Charge]
type ChargeCapture = "v1" :> "charges" :> Id ChargeId :> "capture" :> Body ChargeCaptureReq :> Post' Charge

type CustomerCreate  = "v1" :> "customers" :> Body CustomerCreateReq :> Post' Customer
type CustomerRead    = "v1" :> "customers" :> Id CustomerId :> Get' Customer
type CustomerUpdate  = "v1" :> "customers" :> Id CustomerId :> Body CustomerUpdateReq :> Post' Customer
type CustomerDestroy = "v1" :> "customers" :> Id CustomerId :> Delete' CustomerId
type CustomerList    = "v1" :> "customers" :> GetL [Customer]

type CustomerCardCreate  = "v1" :> "customers" :> Id CustomerId :> "cards" :> Body CardCreateReq :> Post' Card
type CustomerCardRead    = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> Get' Card
type CustomerCardUpdate  = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> Body CardUpdateReq :> Post' Card
type CustomerCardDestroy = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> Delete' CardId
type CustomerCardList    = "v1" :> "customers" :> Id CustomerId :> "cards" :> GetL [Card]

type CustomerBankAccountCreate  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Body BankAccountCreate :> Post' BankAccount
type CustomerBankAccountRead    = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> Get' BankAccount
type CustomerBankAccountUpdate  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> Body BankAccountUpdateReq :> Post' BankAccount
type CustomerBankAccountDestroy = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> Delete' BankAccountId
type CustomerBankAccountList    = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> GetL [BankAccount]
type CustomerBankAccountVerify  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> "verify" :> Body BankAccountVerifyReq :> Post' BankAccount

type PlanCreate  = "v1" :> "plans" :> Body PlanCreateReq :> Post' Plan
type PlanRead    = "v1" :> "plans" :> Id PlanId :> Get' Plan
type PlanUpdate  = "v1" :> "plans" :> Id PlanId :> Body PlanUpdateReq :> Post' Plan
type PlanDestroy = "v1" :> "plans" :> Id PlanId :> Delete' PlanId
type PlanList    = "v1" :> "plans" :> GetL [Plan]

type SubscriptionCreate  = "v1" :> "subscriptions"                      :> Body SubscriptionCreateReq :> Post'    Subscription
type SubscriptionRead    = "v1" :> "subscriptions" :> Id SubscriptionId                               :> Get'     Subscription
-- type SubscriptionUpdate  = "v1" :> "subscriptions" :> Id SubscriptionId :> Body SubscriptionUpdateReq :> Post'    Subscription
type SubscriptionDestroy = "v1" :> "subscriptions" :> Id SubscriptionId                               :> Delete'' Subscription
type SubscriptionList    = "v1" :> "subscriptions"                                                    :> GetL    [Subscription]
type SubscriptionList'   = "v1" :> "subscriptions"                      :> Body SubscriptionListReq   :> GetL    [Subscription]


---- ENDPOINT FUNCTIONS ----

createCustomer  :: Create  CustomerCreateReq             Customer
readCustomer    :: Read    CustomerId                    Customer
updateCustomer  :: Update  CustomerId CustomerUpdateReq  Customer
destroyCustomer :: Destroy CustomerId
listCustomers   :: List                                 [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers =
  client (Proxy :: Proxy CustomerAPI)

createCustomerCard  :: CustomerId -> Create  CardCreateReq         Card
readCustomerCard    :: CustomerId -> Read    CardId                Card
updateCustomerCard  :: CustomerId -> Update  CardId CardUpdateReq  Card
destroyCustomerCard :: CustomerId -> Destroy CardId
listCustomerCards   :: CustomerId -> List                         [Card]
createCustomerCard :<|> readCustomerCard :<|> updateCustomerCard :<|> destroyCustomerCard :<|> listCustomerCards =
  client (Proxy :: Proxy CustomerCardAPI)

createCustomerBankAccount  :: CustomerId -> Create  BankAccountCreate                   BankAccount
readCustomerBankAccount    :: CustomerId -> Read    BankAccountId                       BankAccount
updateCustomerBankAccount  :: CustomerId -> Update  BankAccountId BankAccountUpdateReq  BankAccount
destroyCustomerBankAccount :: CustomerId -> Destroy BankAccountId
listCustomerBankAccounts   :: CustomerId -> List                                       [BankAccount]
verifyCustomerBankAccount  :: CustomerId -> Update  BankAccountId BankAccountVerifyReq  BankAccount
createCustomerBankAccount :<|> readCustomerBankAccount :<|> updateCustomerBankAccount :<|> destroyCustomerBankAccount :<|> listCustomerBankAccounts :<|> verifyCustomerBankAccount =
  client (Proxy :: Proxy CustomerBankAccountAPI)

createPlan  :: Create  PlanCreateReq         Plan
readPlan    :: Read    PlanId                Plan
updatePlan  :: Update  PlanId PlanUpdateReq  Plan
destroyPlan :: Destroy PlanId
listPlans   :: List                         [Plan]
createPlan :<|> readPlan :<|> updatePlan :<|> destroyPlan :<|> listPlans =
  client (Proxy :: Proxy PlanAPI)

createCharge  ::             Create ChargeCreateReq           Charge
readCharge    ::             Read   ChargeId                  Charge
updateCharge  ::             Update ChargeId ChargeUpdateReq  Charge
listCharges   ::             List                            [Charge]
captureCharge :: ChargeId -> Create ChargeCaptureReq          Charge
createCharge :<|> readCharge :<|> updateCharge :<|> listCharges :<|> captureCharge =
  client (Proxy :: Proxy ChargeAPI)

createSubscription  :: Create                  SubscriptionCreateReq  Subscription
readSubscription    :: Read     SubscriptionId                        Subscription
-- updateSubscription  :: Update   SubscriptionId SubscriptionUpdateReq  Subscription
destroySubscription :: Destroy' SubscriptionId                        Subscription -- NOTE: missing `at_period_end` Bool
listSubscriptions   :: List                                          [Subscription]
listSubscriptions'  :: List'                   SubscriptionListReq   [Subscription]
createSubscription :<|> readSubscription :<|> destroySubscription :<|> listSubscriptions :<|> listSubscriptions' =
  client (Proxy :: Proxy SubscriptionAPI)
