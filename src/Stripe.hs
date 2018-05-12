{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-} -- TODO rm? (mv'd to Request.BankAccount)
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Stripe
  ( module Stripe
  , module Stripe.API
  , module Stripe.API.HTTP
  , module Stripe.Types
  , module Stripe.Error
  , module Stripe.Helpers
  , module Stripe.Data.Id -- TODO maybe don't re-export Data.*?
  , module Stripe.Data.BankAccount
  , module Stripe.Data.Card
  , module Stripe.Data.Charge
  , module Stripe.Data.Customer
  , module Stripe.Data.Plan
  , module Stripe.API.Request.BankAccount
  , module Stripe.API.Request.Card
  , module Stripe.API.Request.Charge
  , module Stripe.API.Request.Customer
  , module Stripe.API.Request.Plan
  ) where

import           Stripe.Types
import           Stripe.Error
import           Stripe.Helpers
import           Stripe.Data.Id
import           Stripe.Data.BankAccount
import           Stripe.Data.Card
import           Stripe.Data.Charge
import           Stripe.Data.Customer
import           Stripe.Data.Plan
import           Stripe.API
import           Stripe.API.HTTP
import           Stripe.API.Request.BankAccount
import           Stripe.API.Request.Card
import           Stripe.API.Request.Charge
import           Stripe.API.Request.Customer
import           Stripe.API.Request.Plan



-- Stripe.API      -- API type & funcs
-- Stripe.API.HTTP -- req / resp / data / client -- TODO mv data, e.g. StripeScalar
-- Stripe.Error    -- `Stripe.Data.Error` instead?
