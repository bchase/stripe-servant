module Stripe.Util
  ( fromJsonString
  ) where

import qualified Data.Text               as T

import           Data.Aeson              as J
import           Data.Aeson.Types        as J


fromJsonString :: (T.Text -> a) -> J.Value -> J.Parser a
fromJsonString t (J.String txt) = return . t $ txt
fromJsonString _ _ = mempty
