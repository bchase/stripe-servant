{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Stripe


main :: IO ()
main = do
  stripeList WithoutConnect [ PaginateBy 10 ] listCustomers >>= print
  -- stripe WithoutConnect (createCustomer createReq) >>= print

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
    key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
    -- stripe = stripe' key
    stripeList = stripeList' key
    -- token = Token "tok_mastercard"
    -- createReq = minCustomerCreateReq token
