{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe


main :: IO ()
main = do
  -- CREATE
  let token = Token "tok_mastercard"
      createReq = minCustomerCreateReq token
  (Right createResp) <- stripeScalar WithoutConnect $ createCustomer createReq
  putStrLn . ((++) "[CREATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ createResp

  let custId = customerId . stripeData $ createResp

  -- UPDATE
  let updateReq = emptyCustomerUpdateReq { customerUpdateDescription = Just "test" }
  (Right updateResp) <- stripeScalar WithoutConnect $ updateCustomer custId updateReq
  putStrLn . ((++) "[UPDATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ updateResp

  -- READ
  (Right readResp) <- stripeScalar WithoutConnect (readCustomer custId)
  putStrLn . ((++) "[READ RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ readResp

  -- LIST
  (Right listResp) <- stripeList WithoutConnect [] listCustomers
  putStrLn . ((++) "[LIST RESP] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp

  -- DELETE
  (Right deleteResp) <- stripeDelete WithoutConnect $ destroyCustomer custId
  putStrLn . ((++) "[DELETE RESP] CONTAINS CUSTOMER ID: ") . show $ custId == stripeDeleteId deleteResp

  -- LIST
  (Right listResp') <- stripeList WithoutConnect [] listCustomers
  putStrLn . ((++) "[LIST RESP] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp'

  where
    containsCustomerId id' = any ((==) id' . customerId)

    key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
    stripeScalar = stripeScalar' key
    stripeList = stripeList' key
    stripeDelete = stripeDelete' key
