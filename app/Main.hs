{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe



main :: IO ()
main = do
  let ver    = StripeVersion'2017'08'15
      key    = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
      config = StripeConfig ver key

  eResp <- stripeIO config createAndChargeAndDeleteCustomer

  case eResp of
    Right (cust, charge, charges, gone) -> print charge >> print gone
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err


createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust    <- stripeS WithoutConnect . createCustomer $ custReq
  charge  <- stripeS WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripeL WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- stripeDestroyDeleted <$> (stripeD' WithoutConnect . destroyCustomer $ customerId cust)

  return (cust, charge, charges, deleted)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }
    chargeReq Customer{customerId} = chargeCreateReq 10000 USD (PCustomer customerId)
