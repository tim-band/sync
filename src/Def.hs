module Def (name, parser) where

import qualified Data.HashMap.Strict as Hm
import Data.Text (unpack, pack, Text)
import qualified Data.Yaml as Ym

import Control.Monad.Trans.Class (lift)

import PathFinder (Output, PathFinderO, PathFinder, chain, setState)

name :: String
name = "def"

(.:) :: Ym.FromJSON a => Ym.Object -> String -> Ym.Parser a
(.:) ob s = (Ym..:) ob (pack s)

-- tr :: Ym.Value -> Ym.Parser PathFinder
parser :: PathFinderO
parser tr ob = do
    defsV <- ob .: "let"
    downstreamV <- ob .: "in"
    doDefV defsV downstreamV where
  doDefV :: Ym.Value -> Ym.Value -> Ym.Parser PathFinder
  doDefV defsV downstreamV = Ym.withObject "def.let" (doDef (tr downstreamV)) defsV
  doDef :: Ym.Parser PathFinder -> Ym.Object -> Ym.Parser PathFinder
  doDef down defsH = Hm.foldrWithKey chainSet down defsH
  chainSet :: Text -> Ym.Value -> Ym.Parser PathFinder -> Ym.Parser PathFinder
  chainSet t v p = do
    valuesToSet <- tr v
    lp <- p
    return $ \f -> combine (unpack t) (valuesToSet f) lp
  combine :: String -> Output -> PathFinder -> Output
  combine name pathGetter next = do
    paths <- pathGetter
    setState name paths `chain` next
