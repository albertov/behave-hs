{-# LANGUAGE BangPatterns #-}
module Main where

import Behave
import Data.Maybe (fromJust)
import Arbitrary (Env(..), FuelCode(..))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (generate)
import qualified Data.Vector.Unboxed as U
import Criterion.Main

main :: IO ()
main = defaultMain [
  env (generateVector 10000) $ \ ~vec ->
    let func (e,f) = fromJust (mkSpread standardCatalog f) e
    in bench "spread" $ nf (U.map func) vec
  ]


generateVector :: Int -> IO (U.Vector (SpreadEnv, Int))
generateVector n = U.replicateM n $ do
  Env e <- generate arbitrary
  FuelCode code <- generate arbitrary
  return (e, code)
