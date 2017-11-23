{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stripe


main :: IO ()
main = do
  -- [CUSTOMER] CREATE
  let token = Token "tok_mastercard"
      createReq = minCustomerCreateReq token
  (Right createResp) <- stripeScalar WithoutConnect $ createCustomer createReq
  putStrLn . ((++) "[CREATE CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ createResp

  let custId = customerId . stripeScalarData $ createResp

  -- -- [CUSTOMER] UPDATE
  -- let updateReq = emptyCustomerUpdateReq { customerUpdateDescription = Just "test" }
  -- (Right updateResp) <- stripeScalar WithoutConnect $ updateCustomer custId updateReq
  -- putStrLn . ((++) "[UPDATE CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ updateResp
  --
  -- -- [CUSTOMER] READ
  -- (Right readResp) <- stripeScalar WithoutConnect $ readCustomer custId
  -- putStrLn . ((++) "[READ CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ readResp
  --
  -- -- [CUSTOMER] LIST
  -- (Right listResp) <- stripeList WithoutConnect [] listCustomers
  -- putStrLn . ((++) "[LIST CUSTOMER] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp


  -- -- [CARD] CREATE
  -- let createCardReq = minCardCreateReq . Token $ "tok_visa"
  -- (Right createCardResp) <- stripeScalar WithoutConnect $ createCustomerCard custId createCardReq
  -- putStrLn . ((++) "[CREATE CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ createCardResp
  -- let cardId' = cardId . stripeScalarData $ createCardResp
  -- -- [CARD] UPDATE
  -- let updateCardReq = emptyCardUpdateReq { cardUpdateExpYear = Just 2025 }
  -- (Right updateCardResp) <- stripeScalar WithoutConnect $ updateCustomerCard custId cardId' updateCardReq
  -- putStrLn . ((++) "[UPDATE CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ updateCardResp
  -- -- [CARD] READ
  -- (Right readCardResp) <- stripeScalar WithoutConnect $ readCustomerCard custId cardId'
  -- putStrLn . ((++) "[READ CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ readCardResp
  -- -- [CARD] LIST
  -- (Right listCardsResp) <- stripeList WithoutConnect [] $ listCustomerCards custId
  -- putStrLn . ((++) "[LIST CARDS] `last4`s: ") . show . map cardLast4 . stripeListData $ listCardsResp
  -- -- [CARD] DESTROY
  -- (Right deleteCardResp) <- stripeDelete WithoutConnect $ destroyCustomerCard custId cardId'
  -- putStrLn . ((++) "[DESTROY CARD] `deleted`: ") . show . stripeDestroyDeleted $ deleteCardResp
  -- -- [CARD] LIST
  -- (Right listCardsResp') <- stripeList WithoutConnect [] $ listCustomerCards custId
  -- putStrLn . ((++) "[LIST CARDS] `last4`s: ") . show . map cardLast4 . stripeListData $ listCardsResp'


  -- [BANK ACCOUNT] CREATE
  token' <- getTestBankAccountToken key
  let createBankAccountReq = minBankAccountCreateReq token'
  (Right createBankAccountResp) <- stripeScalar WithoutConnect $ createCustomerBankAccount custId createBankAccountReq
  putStrLn . ((++) "[CREATE BANK ACCOUNT] `last4`: ") . show . bankAccountLast4 . stripeScalarData $ createBankAccountResp
  -- let bankAccountId' = bankAccountId . stripeScalarData $ createBankAccountResp
  -- -- [BANK ACCOUNT] UPDATE
  -- let updateBankAccountReq = emptyBankAccountUpdateReq { bankAccountUpdateExpYear = Just 2025 }
  -- (Right updateBankAccountResp) <- stripeScalar WithoutConnect $ updateCustomerBankAccount custId bankAccountId' updateBankAccountReq
  -- putStrLn . ((++) "[UPDATE BANK ACCOUNT] `expYear`: ") . show . bankAccountExpYear . stripeScalarData $ updateBankAccountResp
  -- -- [BANK ACCOUNT] READ
  -- (Right readBankAccountResp) <- stripeScalar WithoutConnect $ readCustomerBankAccount custId bankAccountId'
  -- putStrLn . ((++) "[READ BANK ACCOUNT] `expYear`: ") . show . bankAccountExpYear . stripeScalarData $ readBankAccountResp
  -- -- [BANK ACCOUNT] LIST
  -- (Right listBankAccountsResp) <- stripeList WithoutConnect [] $ listCustomerBankAccounts custId
  -- putStrLn . ((++) "[LIST BANK ACCOUNTS] `last4`s: ") . show . map bankAccountLast4 . stripeListData $ listBankAccountsResp
  -- -- [BANK ACCOUNT] DESTROY
  -- (Right deleteBankAccountResp) <- stripeDelete WithoutConnect $ destroyCustomerBankAccount custId bankAccountId'
  -- putStrLn . ((++) "[DESTROY BANK ACCOUNT] `deleted`: ") . show . stripeDestroyDeleted $ deleteBankAccountResp
  -- [BANK ACCOUNT] LIST
  (Right listBankAccountsResp') <- stripeList WithoutConnect [] $ listCustomerBankAccounts custId
  putStrLn . ((++) "[LIST BANK ACCOUNTS] `last4`s: ") . show . map bankAccountLast4 . stripeListData $ listBankAccountsResp'


  -- -- [CUSTOMER] DESTROY
  -- (Right deleteResp) <- stripeDelete WithoutConnect $ destroyCustomer custId
  -- putStrLn . ((++) "[DESTROY CUSTOMER] CONTAINS CUSTOMER ID: ") . show $ custId == stripeDestroyId deleteResp
  --
  -- -- [CUSTOMER] LIST
  -- (Right listResp') <- stripeList WithoutConnect [] listCustomers
  -- putStrLn . ((++) "[LIST CUSTOMER] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp'

  where
    -- containsCustomerId id' = any ((==) id' . customerId)

    key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
    stripeScalar = stripeScalar' key
    stripeList = stripeList' key
    -- stripeDelete = stripeDelete' key
