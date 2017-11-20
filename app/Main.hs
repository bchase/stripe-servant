{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe


main :: IO ()
main = do
  -- CREATE
  (Right createResp) <- stripe WithoutConnect $ createCustomer createReq
  putStrLn . ((++) "[CREATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ createResp

  let custId = customerId . stripeData $ createResp

  -- UPDATE
  let updateReq = emptyCustomerUpdateReq { customerUpdateDescription = Just "test" }
  (Right updateResp) <- stripe WithoutConnect $ updateCustomer custId updateReq
  putStrLn . ((++) "[UPDATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ updateResp

  -- READ
  (Right readResp) <- stripe WithoutConnect (readCustomer custId)
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
    containsCustomerId id' = any (\Customer{customerId} -> customerId == id')

    key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
    stripe = stripe' key
    stripeList = stripeList' key
    stripeDelete = stripeDelete' key
    token = Token "tok_mastercard"
    createReq = minCustomerCreateReq token
