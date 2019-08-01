{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment (getEnv)
import qualified Data.Text          as T

import           Stripe



createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust    <- stripe WithoutConnect . createCustomer $ custReq
  charge  <- stripe WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripe WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- fmap (destroyDeleted . stripeMetadata) . stripe' WithoutConnect . destroyCustomer $ customerId cust

  return (cust, charge, charges, deleted)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

    chargeReq Customer{customerId} = chargeCreateReq (Price USD 10000) (SCustomer customerId)


main :: IO ()
main = do
  key <- SecretKey . T.pack <$> getEnv "STRIPE_CLIENT_SECRET"

  let ver    = Version'2017'08'15
      config = Config ver key

  eResp <- stripeIO config createAndChargeAndDeleteCustomer

  case eResp of
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
    Right (_, charge, _, _)             -> print charge
