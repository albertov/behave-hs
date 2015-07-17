{-# LANGUAGE BangPatterns #-}
module Main where

import Behave
import Data.Maybe (fromJust)
import Arbitrary (Env(..), FuelCode(..))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (generate)
import qualified Data.Vector.Unboxed as U
import Control.DeepSeq (deepseq)
import System.TimeIt

main :: IO ()
main = do
  !v <- generateVector 4367379
  timeIt $ do
    let sps = U.map (\(env,code) -> 
                fromJust (mkSpread standardCatalog code) env) v
    print (sps `deepseq` "done")
  


generateVector :: Int -> IO (U.Vector (SpreadEnv, Int))
generateVector n = U.replicateM n $ do
  Env env <- generate arbitrary
  FuelCode code <- generate arbitrary
  return (env, code)
