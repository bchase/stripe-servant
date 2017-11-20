{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe


main :: IO ()
main = do
  -- stripeList WithoutConnect [ PaginateBy 10 ] listCustomers >>= print
  -- -- stripe WithoutConnect (createCustomer createReq)          >>= print

  -- CREATE
  (Right createResp) <- stripe WithoutConnect (createCustomer createReq)
  putStrLn . ((++) "[CREATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ createResp

  -- UPDATE
  let custId = customerId . stripeData $ createResp
      updateReq = emptyCustomerUpdateReq { customerUpdateDescription = Just "test" }
  (Right updateResp) <- stripe WithoutConnect (updateCustomer custId updateReq)
  putStrLn . ((++) "[UPDATE RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ updateResp

  -- READ
  (Right readResp) <- stripe WithoutConnect (readCustomer custId)
  putStrLn . ((++) "[READ RESP] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeData $ readResp

  -- LIST
  (Right listResp) <- stripeList WithoutConnect [] listCustomers
  putStrLn . ((++) "[LIST RESP] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp

  -- -- DELETE
  -- (Right listResp) <- stripeList WithoutConnect [] listCustomers
  -- putStrLn . ((++) "[DELETE RESP] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp
  --
  -- -- LIST
  -- (Right listResp) <- stripeList WithoutConnect [] listCustomers
  -- putStrLn . ((++) "[LIST RESP] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp



  -- -- create customer
  -- eCustomer <- stripe WithoutConnect (createCustomer createReq)
  -- case eCustomer of
  --   Right s ->
  --     print . customerId . sData $ s
  --   err ->
  --     print err
  --
  -- -- update customer description
  -- stripe WithoutConnect (createCustomer createReq)
  --
  -- -- read customer and check new description
  -- stripe WithoutConnect (createCustomer createReq)
  --
  -- -- list customers and check for customer
  -- stripeList WithoutConnect [ PaginateBy 10 ] listCustomers
  --
  -- -- destroy customer
  -- stripe WithoutConnect (createCustomer createReq)
  --
  -- -- list customers and check for customer
  -- stripeList WithoutConnect [ PaginateBy 10 ] listCustomers

  return ()

  where
    containsCustomerId id' = any (\Customer{customerId} -> customerId == id')

    key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
    stripe = stripe' key
    stripeList = stripeList' key
    token = Token "tok_mastercard"
    createReq = minCustomerCreateReq token
