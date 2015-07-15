{-# LANGUAGE RecordWildCards #-}
module BehaveSpec (main, spec) where

import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), choose)
import HsFirelib (standardSpread)
import Debug.Trace
import Behave

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spread" $ do
    prop "behaves like fireLib" $ \(FuelCode code, Env env) ->
      let Just fuel = catalogIndex standardCatalog code
          computed  = spread fuel env
          expected  = standardSpread code env
      in traceShow (computed, expected) (computed `spreadEq` expected)

spreadEq :: Spread -> Spread -> Bool
spreadEq a b = all id [
    spreadRxInt a `almostEq` spreadRxInt b
  , spreadSpeed0 a `almostEq` spreadSpeed0 b
  , spreadHpua a `almostEq` spreadHpua b
  ]
  where almostEq a' b' = abs (a'-b') < tolerance
        tolerance = 1e-6

newtype FuelCode = FuelCode Int deriving (Eq, Show)
newtype Env = Env SpreadEnv deriving (Eq, Show)

instance Arbitrary FuelCode where
  arbitrary = FuelCode <$> choose (0,13)

instance Arbitrary Env where
  arbitrary = Env <$> spreadEnv
    where spreadEnv = SpreadEnv <$> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,100)
                                <*> choose (0,359)
                                <*> choose (0,1)
                                <*> choose (0,359)
