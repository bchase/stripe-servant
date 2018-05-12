{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe



createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust    <- stripeS WithoutConnect . createCustomer $ custReq
  charge  <- stripeS WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripeL WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- fmap stripeDestroyDeleted . stripeD' WithoutConnect . destroyCustomer $ customerId cust

  return (cust, charge, charges, deleted)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

    chargeReq Customer{customerId} = chargeCreateReq (Price USD 10000) (SCustomer customerId)


main :: IO ()
main = do
  let ver    = Version'2017'08'15
      key    = SecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
      config = Config ver key

  eResp <- stripeIO config createAndChargeAndDeleteCustomer

  case eResp of
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
    Right (_, charge, _, _)             -> print charge
