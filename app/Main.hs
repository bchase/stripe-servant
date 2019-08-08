{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment     (getEnv)

import qualified Data.Text as T

import Stripe


createCustomerAndSubscribeToPlan :: Stripe (Customer, Plan, Subscription, (Bool, Bool, Bool, Bool, Int))
createCustomerAndSubscribeToPlan = do
  cust  <- stripe WithoutConnect . createCustomer $ custReq
  cust' <- stripe WithoutConnect . createCustomer $ custReq

  plans <- stripe WithoutConnect . paginate [ By 10 ] $ listPlans

  let (plan1:_) = plans

  sub    <- stripe WithoutConnect . createSubscription . subReq cust $ planId plan1
  sub'   <- stripe WithoutConnect . readSubscription $ subscriptionId sub
  subs   <- stripe WithoutConnect . paginate [] $ listSubscriptions
  _      <- stripe WithoutConnect . destroySubscription $ subscriptionId sub
  subs'  <- stripe WithoutConnect . paginate [] $ listSubscriptions


  let t = Time 1565259120 undefined
      f = TimeFilterGT (GTE t)
  subs'' <- stripe WithoutConnect . paginate [] $ listSubscriptions' (subscriptionListReq { subscriptionListCustomer = Just $ customerId cust', subscriptionListCreated = Just f })

  let id' = subscriptionId sub
      b1  = id' == subscriptionId sub'
      b2  = any (== id') . map subscriptionId $ subs
      b3  = all (/= id') . map subscriptionId $ subs'
      b4  = all (/= customerId cust) . map (subscriptionCustomer) $ subs''

  return (cust, plan1, sub, (b1, b2, b3, b4, length subs''))

  where
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
    Right (_, _, sub, results)          -> print (sub, results)


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
