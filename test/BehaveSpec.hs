{-# LANGUAGE RecordWildCards #-}
module BehaveSpec (main, spec) where

import Test.Hspec (Spec, hspec, describe, shouldBe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), choose)
import Data.Maybe (fromJust)
import HsFirelib (standardSpread)
import Debug.Trace
import Behave

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spread" $ do
    prop "behaves like fireLib" $ \(FuelCode code, ArbEnv env) ->
      let Just fuel = catalogIndex standardCatalog code
          computed  = spread fuel env
          expected  = standardSpread code env

      in traceShow (computed, expected) (computed==expected)

newtype FuelCode = FuelCode Int deriving (Eq, Show)
newtype ArbEnv = ArbEnv SpreadEnv deriving (Eq, Show)

instance Arbitrary FuelCode where
  arbitrary = FuelCode <$> choose (0,13)

instance Arbitrary ArbEnv where
  arbitrary = ArbEnv <$> spreadEnv
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
