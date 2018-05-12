module Stripe.Util
  ( deriveFromJSON'
  , fromJsonString
  ) where

import qualified Data.Text                   as T
import           Language.Haskell.TH.Syntax  as TH

import           Data.Aeson.Casing  (snakeCase)
import           Data.Aeson         as J
import           Data.Aeson.Types   as J
import           Data.Aeson.TH      (Options (..), defaultOptions, deriveFromJSON)



fromJsonString :: (T.Text -> a) -> J.Value -> J.Parser a
fromJsonString t (J.String txt) = return . t $ txt
fromJsonString _ _ = mempty


deriveFromJSON' :: TH.Name -> TH.Q [TH.Dec]
deriveFromJSON' n = deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop (moduleNameLength n) } n where
  moduleNameLength = length . takeWhile (/= '.') . reverse . show
