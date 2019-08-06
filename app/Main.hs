{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment     (getEnv)

import qualified Data.Text as T

import Stripe


createCustomerAndSubscribeToPlan :: Stripe (Customer, Plan, Subscription)
createCustomerAndSubscribeToPlan = do
  cust  <- stripe WithoutConnect . createCustomer $ custReq
  -- _     <- stripe WithoutConnect . createPlan $ planReq
  plans <- stripe WithoutConnect . paginate [ By 10 ] $ listPlans

  let plan@Plan{planId} = head plans -- TODO !!!
  liftIO $ print plan

  sub <- stripe WithoutConnect . createSubscription $ subReq cust planId

  return (cust, plan, sub)

  where
    -- planReq = planCreateReq (PlanId "test") "test" (Price USD 1000) Month

    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

    subReq Customer{customerId} plan = subscriptionCreateReq customerId $
      [ subItem' plan 2
      ]


main :: IO ()
main = do
  key <- SecretKey . T.pack <$> getEnv "STRIPE_CLIENT_SECRET"

  let ver    = Version'2017'08'15
      config = Config ver key

  eResp <- stripeIO config createCustomerAndSubscribeToPlan

  case eResp of
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
    Right (_, _, sub)                   -> print sub


-- createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
-- createAndChargeAndDeleteCustomer = do
--   cust    <- stripe WithoutConnect . createCustomer $ custReq
--   charge  <- stripe WithoutConnect . createCharge   $ chargeReq cust

--   charges <- stripe WithoutConnect . paginate [ By 10 ] $ listCharges

--   deleted <-  (destroyDeleted . stripeMetadata)
--           <$> (stripe' WithoutConnect . destroyCustomer $ customerId cust)

--   return (cust, charge, charges, deleted)

--   where
--     custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

--     chargeReq Customer{customerId} = chargeCreateReq (Price USD 10000) (SCustomer customerId)


-- main :: IO ()
-- main = do
--   key <- SecretKey . T.pack <$> getEnv "STRIPE_CLIENT_SECRET"

--   let ver    = Version'2017'08'15
--       config = Config ver key

--   eResp <- stripeIO config createAndChargeAndDeleteCustomer

--   case eResp of
--     Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
--     Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
--     Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
--     Right (_, charge, _, _)             -> print charge
