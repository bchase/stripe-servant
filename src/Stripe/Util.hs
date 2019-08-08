module Stripe.Util
  ( deriveFromJSON'
  , fromJsonString
  , addToForm
  ) where

import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (Options (..), defaultOptions, deriveFromJSON)
import Data.Text (Text)

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Language.Haskell.TH.Syntax as TH
import qualified Web.Internal.FormUrlEncoded as F



fromJsonString :: (T.Text -> a) -> J.Value -> J.Parser a
fromJsonString t (J.String txt) = return . t $ txt
fromJsonString _ _ = mempty


deriveFromJSON' :: TH.Name -> TH.Q [TH.Dec]
deriveFromJSON' n = deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase . drop (moduleNameLength n) } n where
  moduleNameLength = length . takeWhile (/= '.') . reverse . show


addToForm :: Text -> [(Text, [Text])] -> F.Form -> F.Form
addToForm key fs form = F.Form . HM.union hm' $ hm
  where
    hm  = HM.delete key . F.unForm $ form
    hm' = HM.fromList fs
