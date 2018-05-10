{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Text              as T
import           Data.Time.Clock        as Time
import           Data.Time.Clock.POSIX  as Time
import           Control.Monad.IO.Class (liftIO)

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


createAndChargeAndDeleteCustomer :: S (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust   <- stripeS WithoutConnect . createCustomer $ custReq
  charge <- stripeS WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripeL WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- stripeDestroyDeleted <$> (stripeD' WithoutConnect . destroyCustomer $ customerId cust)

  return (cust, charge, charges, deleted)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }
    chargeReq Customer{customerId} = chargeCreateReq 10000 USD (PCustomer customerId)




-- main :: IO ()
-- main = do
--   ---- PLANS ----
--   s <- Time.getCurrentTime >>= return . T.pack . show . Time.utcTimeToPOSIXSeconds
--   -- [PLAN] CREATE
--   let createPlanReq =
--         PlanCreateReq
--           { planCreateId                  = PlanId . mconcat $ ["test-plan-", s]
--           , planCreateName                = "Test Plan Name"
--           , planCreateAmount              = 1000
--           , planCreateCurrency            = USD
--           , planCreateInterval            = Month
--           , planCreateIntervalCount       = Just 1
--           , planCreateStatementDescriptor = Just "Company -- Test Plan" -- TODO length txt <= 22
--           , planCreateTrialPeriodDays     = Nothing
--           , planCreateMetadata            = Just . metadata $ [("test-key", "test-val")]
--           }
--   -- stripeScalar WithoutConnect (createPlan createPlanReq) >>= print
--   (Right createPlanResp) <- stripeScalar WithoutConnect $ createPlan createPlanReq
--   putStrLn . ((++) "[CREATE PLAN] PLAN ID: ") . show . unPlanId . planId . stripeScalarData $ createPlanResp
--   let planId' = planId . stripeScalarData $ createPlanResp
--   -- [PLAN] UPDATE
--   let updatePlanReq = emptyPlanUpdateReq { planUpdateName = Just "Updated Test Plan Name" }
--   (Right updatePlanResp) <- stripeScalar WithoutConnect $ updatePlan planId' updatePlanReq
--   putStrLn . ((++) "[UPDATE PLAN] PLAN Name: ") . show . planName . stripeScalarData $ updatePlanResp
--   -- [PLAN] READ
--   (Right readPlanResp) <- stripeScalar WithoutConnect $ readPlan planId'
--   putStrLn . ((++) "[READ PLAN] PLAN NAME: ") . show . planName . stripeScalarData $ readPlanResp
--   -- [PLAN] LIST
--   (Right listPlanResp) <- stripeList WithoutConnect [] listPlans
--   putStrLn . ((++) "[LIST PLAN] PLAN IDS: ") . show . map (unPlanId . planId) . stripeListData $ listPlanResp
--   -- [PLAN] DESTROY
--   (Right deletePlanResp) <- stripeDelete WithoutConnect $ destroyPlan planId'
--   putStrLn . ((++) "[DESTROY PLAN] DELETED: ") . show .stripeDestroyDeleted $ deletePlanResp
--   -- [PLAN] LIST
--   (Right listPlanResp') <- stripeList WithoutConnect [] listPlans
--   putStrLn . ((++) "[LIST PLAN] PLAN IDS: ") . show . map (unPlanId . planId) . stripeListData $ listPlanResp'
--   -- stripeList WithoutConnect [] listPlans >>= print
--
--
--   -- [CUSTOMER] CREATE
--   let token = Token "tok_mastercard"
--       createReq = minCustomerCreateReq token
--   (Right createResp) <- stripeScalar WithoutConnect $ createCustomer createReq
--   putStrLn . ((++) "[CREATE CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ createResp
--
--   let custId = customerId . stripeScalarData $ createResp
--
--   -- [CUSTOMER] UPDATE
--   let updateReq = emptyCustomerUpdateReq { customerUpdateDescription = Just "test" }
--   (Right updateResp) <- stripeScalar WithoutConnect $ updateCustomer custId updateReq
--   putStrLn . ((++) "[UPDATE CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ updateResp
--
--   -- [CUSTOMER] READ
--   (Right readResp) <- stripeScalar WithoutConnect $ readCustomer custId
--   putStrLn . ((++) "[READ CUSTOMER] CUSTOMER DESCRIPTION: ") . show . customerDescription . stripeScalarData $ readResp
--
--   -- [CUSTOMER] LIST
--   (Right listResp) <- stripeList WithoutConnect [] listCustomers
--   putStrLn . ((++) "[LIST CUSTOMER] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp
--
--
--   -- [CHARGE] CREATE
--   let createChargeReq = (chargeCreateReq 10000 USD (PToken $ Token "tok_visa")) { chargeCreateCapture = Just False }
--   (Right createChargeResp) <- stripeScalar WithoutConnect $ createCharge createChargeReq
--   putStrLn . ((++) "[CREATE CHARGE] `captured`: ") . show . chargeCaptured . stripeScalarData $ createChargeResp
--   let chargeId' = chargeId . stripeScalarData $ createChargeResp
--   -- [CHARGE] READ
--   (Right readChargeResp) <- stripeScalar WithoutConnect $ readCharge chargeId'
--   putStrLn . ((++) "[READ CHARGE] `chargeDescription`: ") . show . chargeDescription . stripeScalarData $ readChargeResp
--   -- [CHARGE] UPDATE
--   let updateChargeReq = emptyChargeUpdateReq { chargeUpdateDescription = Just "chargeUpdate test" }
--   (Right updateChargeResp) <- stripeScalar WithoutConnect $ updateCharge chargeId' updateChargeReq
--   putStrLn . ((++) "[UPDATE CHARGE] `chargeDescription`: ") . show . chargeDescription . stripeScalarData $ updateChargeResp
--   -- [CHARGE] LIST
--   (Right listChargesResp) <- stripeList WithoutConnect [] $ listCharges
--   putStrLn . ((++) "[LIST CHARGES] amounts: ") . show . map (\c ->  (show $ chargeCurrency c) ++ (show $ chargeAmount c)) . stripeListData $ listChargesResp
--   -- [CHARGE] CAPTURE
--   (Right captureChargeResp) <- stripeScalar WithoutConnect $ captureCharge chargeId' chargeCaptureReq
--   putStrLn . ((++) "[CAPTURE CHARGE] `captured`: ") . show . chargeCaptured . stripeScalarData $ captureChargeResp
--
--
--   -- [CARD] CREATE
--   let createCardReq = minCardCreateReq . Token $ "tok_visa"
--   (Right createCardResp) <- stripeScalar WithoutConnect $ createCustomerCard custId createCardReq
--   putStrLn . ((++) "[CREATE CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ createCardResp
--   let cardId' = cardId . stripeScalarData $ createCardResp
--   -- [CARD] UPDATE
--   let updateCardReq = emptyCardUpdateReq { cardUpdateExpYear = Just 2025 }
--   (Right updateCardResp) <- stripeScalar WithoutConnect $ updateCustomerCard custId cardId' updateCardReq
--   putStrLn . ((++) "[UPDATE CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ updateCardResp
--   -- [CARD] READ
--   (Right readCardResp) <- stripeScalar WithoutConnect $ readCustomerCard custId cardId'
--   putStrLn . ((++) "[READ CARD] `expYear`: ") . show . cardExpYear . stripeScalarData $ readCardResp
--   -- [CARD] LIST
--   (Right listCardsResp) <- stripeList WithoutConnect [] $ listCustomerCards custId
--   putStrLn . ((++) "[LIST CARDS] `last4`s: ") . show . map cardLast4 . stripeListData $ listCardsResp
--   -- [CARD] DESTROY
--   (Right deleteCardResp) <- stripeDelete WithoutConnect $ destroyCustomerCard custId cardId'
--   putStrLn . ((++) "[DESTROY CARD] `deleted`: ") . show . stripeDestroyDeleted $ deleteCardResp
--   -- [CARD] LIST
--   (Right listCardsResp') <- stripeList WithoutConnect [] $ listCustomerCards custId
--   putStrLn . ((++) "[LIST CARDS] `last4`s: ") . show . map cardLast4 . stripeListData $ listCardsResp'
--
--
--   -- [BANK ACCOUNT] CREATE
--   token' <- getTestBankAccountToken
--   let createBankAccountReq = minBankAccountCreateReq token'
--   (Right createBankAccountResp) <- stripeScalar WithoutConnect $ createCustomerBankAccount custId createBankAccountReq
--   putStrLn . ((++) "[CREATE BANK ACCOUNT] `accountHolderName`: ") . show . bankAccountAccountHolderName . stripeScalarData $ createBankAccountResp
--   let bankAccountId' = bankAccountId . stripeScalarData $ createBankAccountResp
--   -- [BANK ACCOUNT] UPDATE
--   let updateBankAccountReq = emptyBankAccountUpdateReq { bankAccountUpdateAccountHolderName = Just "Olivia Smith" }
--   (Right updateBankAccountResp) <- stripeScalar WithoutConnect $ updateCustomerBankAccount custId bankAccountId' updateBankAccountReq
--   putStrLn . ((++) "[UPDATE BANK ACCOUNT] `accountHolderName`: ") . show . bankAccountAccountHolderName . stripeScalarData $ updateBankAccountResp
--   -- [BANK ACCOUNT] READ
--   (Right readBankAccountResp) <- stripeScalar WithoutConnect $ readCustomerBankAccount custId bankAccountId'
--   putStrLn . ((++) "[READ BANK ACCOUNT] `accountHolderName`: ") . show . bankAccountAccountHolderName . stripeScalarData $ readBankAccountResp
--   putStrLn . ((++) "[READ BANK ACCOUNT] `status`: ") . show . bankAccountStatus . stripeScalarData $ readBankAccountResp
--   -- [BANK ACCOUNT] VERIFY
--   (Right verifyBankAccountResp) <- stripeScalar WithoutConnect $ verifyCustomerBankAccount custId bankAccountId' $ BankAccountVerifyReq 32 45
--   putStrLn . ((++) "[VERIFY BANK ACCOUNT] `status`: ") . show . bankAccountStatus . stripeScalarData $ verifyBankAccountResp
--   -- [BANK ACCOUNT] READ
--   (Right readBankAccountResp') <- stripeScalar WithoutConnect $ readCustomerBankAccount custId bankAccountId'
--   putStrLn . ((++) "[READ BANK ACCOUNT] `accountHolderName`: ") . show . bankAccountAccountHolderName . stripeScalarData $ readBankAccountResp'
--   putStrLn . ((++) "[READ BANK ACCOUNT] `status`: ") . show . bankAccountStatus . stripeScalarData $ readBankAccountResp'
--   -- [BANK ACCOUNT] LIST
--   (Right listBankAccountsResp) <- stripeList WithoutConnect [] $ listCustomerBankAccounts custId
--   putStrLn . ((++) "[LIST BANK ACCOUNTS] `accountHolderName`s: ") . show . map bankAccountAccountHolderName . stripeListData $ listBankAccountsResp
--   -- [BANK ACCOUNT] DESTROY
--   (Right deleteBankAccountResp) <- stripeDelete WithoutConnect $ destroyCustomerBankAccount custId bankAccountId'
--   putStrLn . ((++) "[DESTROY BANK ACCOUNT] `deleted`: ") . show . stripeDestroyDeleted $ deleteBankAccountResp
--   -- [BANK ACCOUNT] LIST
--   (Right listBankAccountsResp') <- stripeList WithoutConnect [] $ listCustomerBankAccounts custId
--   putStrLn . ((++) "[LIST BANK ACCOUNTS] `accountHolderName`s: ") . show . map bankAccountAccountHolderName . stripeListData $ listBankAccountsResp'
--
--
--   -- [CUSTOMER] DESTROY
--   (Right deleteResp) <- stripeDelete WithoutConnect $ destroyCustomer custId
--   putStrLn . ((++) "[DESTROY CUSTOMER] CONTAINS CUSTOMER ID: ") . show $ custId == stripeDestroyId deleteResp
--
--   -- [CUSTOMER] LIST
--   (Right listResp') <- stripeList WithoutConnect [] listCustomers
--   putStrLn . ((++) "[LIST CUSTOMER] CONTAINS CUSTOMER ID: ") . show . containsCustomerId custId . stripeListData $ listResp'
--
--   where
--     containsCustomerId id' = any ((==) id' . customerId)
--
--     key = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
--     stripeScalar = stripeScalar' key
--     stripeList = stripeList' key
--     stripeDelete = stripeDelete' key
--
--     getTestBankAccountToken :: IO Token
--     getTestBankAccountToken = do
--       resp <- stripeScalar WithoutConnect $ createBankAccountToken testBankAccountTokenCreateReq
--       case resp of
--         Left  err -> putStrLn "[[FAIL Stripe.getTestBankAccountToken]]" >> print err >> error ""
--         Right bat -> return . bankAccountTokenId . stripeScalarData $ bat
--       where
--         testBankAccountTokenCreateReq =
--           BankAccountTokenCreateReq
--             { bankAccountTokenCreateCountry           = "US"
--             , bankAccountTokenCreateCurrency          = "usd"
--             , bankAccountTokenCreateAccountHolderName = "Olivia Harris"
--             , bankAccountTokenCreateAccountHolderType = "individual"
--             , bankAccountTokenCreateRoutingNumber     = "110000000"
--             , bankAccountTokenCreateAccountNumber     = "000123456789"
--             }
