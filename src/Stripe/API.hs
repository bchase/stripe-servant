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
import           Stripe.API.Request.BankAccount
import           Stripe.API.Request.Card
import           Stripe.API.Request.Charge
import           Stripe.API.Request.Customer
import           Stripe.API.Request.Plan
import           Stripe.API.HTTP (Id, StripeHeaders, Body,
                                  StripePaginationQueryParams,
                                  Create, Read, Update, Destroy, List,
                                  GetJS, GetJL, PostJ, DeleteJ)



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


type ChargeCreate  = "v1" :> "charges" :> Body ChargeCreateReq :> StripeHeaders (PostJ Charge)
type ChargeRead    = "v1" :> "charges" :> Id ChargeId :> StripeHeaders (GetJS Charge)
type ChargeUpdate  = "v1" :> "charges" :> Id ChargeId :> Body ChargeUpdateReq :> StripeHeaders (PostJ Charge)
type ChargeList    = "v1" :> "charges" :> StripePaginationQueryParams (StripeHeaders (GetJL [Charge]))
type ChargeCapture = "v1" :> "charges" :> Id ChargeId :> "capture" :> Body ChargeCaptureReq :> StripeHeaders (PostJ Charge)

type CustomerCreate  = "v1" :> "customers" :> Body CustomerCreateReq :> StripeHeaders (PostJ Customer)
type CustomerRead    = "v1" :> "customers" :> Id CustomerId :> StripeHeaders (GetJS Customer)
type CustomerUpdate  = "v1" :> "customers" :> Id CustomerId :> Body CustomerUpdateReq :> StripeHeaders (PostJ Customer)
type CustomerDestroy = "v1" :> "customers" :> Id CustomerId :> StripeHeaders (DeleteJ CustomerId)
type CustomerList    = "v1" :> "customers" :> StripePaginationQueryParams (StripeHeaders (GetJL [Customer]))

type CustomerCardCreate  = "v1" :> "customers" :> Id CustomerId :> "cards" :> Body CardCreateReq :> StripeHeaders (PostJ Card)
type CustomerCardRead    = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> StripeHeaders (GetJS Card)
type CustomerCardUpdate  = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> Body CardUpdateReq :> StripeHeaders (PostJ Card)
type CustomerCardDestroy = "v1" :> "customers" :> Id CustomerId :> "cards" :> Id CardId :> StripeHeaders (DeleteJ CardId)
type CustomerCardList    = "v1" :> "customers" :> Id CustomerId :> "cards" :> StripePaginationQueryParams (StripeHeaders (GetJL [Card]))

type CustomerBankAccountCreate  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Body BankAccountCreate :> StripeHeaders (PostJ BankAccount)
type CustomerBankAccountRead    = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> StripeHeaders (GetJS BankAccount)
type CustomerBankAccountUpdate  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> Body BankAccountUpdateReq :> StripeHeaders (PostJ BankAccount)
type CustomerBankAccountDestroy = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> StripeHeaders (DeleteJ BankAccountId)
type CustomerBankAccountList    = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> StripePaginationQueryParams (StripeHeaders (GetJL [BankAccount]))
type CustomerBankAccountVerify  = "v1" :> "customers" :> Id CustomerId :> "bank_accounts" :> Id BankAccountId :> "verify" :> Body BankAccountVerifyReq :> StripeHeaders (PostJ BankAccount)

type PlanCreate  = "v1" :> "plans" :> Body PlanCreateReq :> StripeHeaders (PostJ Plan)
type PlanRead    = "v1" :> "plans" :> Id PlanId :> StripeHeaders (GetJS Plan)
type PlanUpdate  = "v1" :> "plans" :> Id PlanId :> Body PlanUpdateReq :> StripeHeaders (PostJ Plan)
type PlanDestroy = "v1" :> "plans" :> Id PlanId :> StripeHeaders (DeleteJ PlanId)
type PlanList    = "v1" :> "plans" :> StripePaginationQueryParams (StripeHeaders (GetJL [Plan]))



---- ENDPOINT FUNCTIONS ----

createCustomer :: Create CustomerCreateReq Customer
readCustomer :: Read CustomerId Customer
updateCustomer :: Update CustomerId CustomerUpdateReq Customer
destroyCustomer :: Destroy CustomerId
listCustomers :: List [Customer]
createCustomer :<|> readCustomer :<|> updateCustomer :<|> destroyCustomer :<|> listCustomers =
  client (Proxy :: Proxy CustomerAPI)

createCustomerCard :: CustomerId -> Create CardCreateReq Card
readCustomerCard :: CustomerId -> Read CardId Card
updateCustomerCard :: CustomerId -> Update CardId CardUpdateReq Card
destroyCustomerCard :: CustomerId -> Destroy CardId
listCustomerCards :: CustomerId -> List [Card]
createCustomerCard :<|> readCustomerCard :<|> updateCustomerCard :<|> destroyCustomerCard :<|> listCustomerCards =
  client (Proxy :: Proxy CustomerCardAPI)

createCustomerBankAccount :: CustomerId -> Create BankAccountCreate BankAccount
readCustomerBankAccount :: CustomerId -> Read BankAccountId BankAccount
updateCustomerBankAccount :: CustomerId -> Update BankAccountId BankAccountUpdateReq BankAccount
destroyCustomerBankAccount :: CustomerId -> Destroy BankAccountId
listCustomerBankAccounts :: CustomerId -> List [BankAccount]
verifyCustomerBankAccount :: CustomerId -> Update BankAccountId BankAccountVerifyReq BankAccount
createCustomerBankAccount :<|> readCustomerBankAccount :<|> updateCustomerBankAccount :<|> destroyCustomerBankAccount :<|> listCustomerBankAccounts :<|> verifyCustomerBankAccount =
  client (Proxy :: Proxy CustomerBankAccountAPI)

createPlan :: Create PlanCreateReq Plan
readPlan :: Read PlanId Plan
updatePlan :: Update PlanId PlanUpdateReq Plan
destroyPlan :: Destroy PlanId
listPlans :: List [Plan]
createPlan :<|> readPlan :<|> updatePlan :<|> destroyPlan :<|> listPlans =
  client (Proxy :: Proxy PlanAPI)

createCharge :: Create ChargeCreateReq Charge
readCharge :: Read ChargeId Charge
updateCharge :: Update ChargeId ChargeUpdateReq Charge
listCharges :: List [Charge]
captureCharge :: ChargeId -> Create ChargeCaptureReq Charge
createCharge :<|> readCharge :<|> updateCharge :<|> listCharges :<|> captureCharge =
  client (Proxy :: Proxy ChargeAPI)
