{-# LANGUAGE RecordWildCards #-}
module BehaveSpec (main, spec) where

import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), choose)
import HsFirelib (standardSpread)
--import Debug.Trace
import Behave

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spread" $ do
    prop "behaves like fireLib" $ \(FuelCode code, Env env, Azimuth az) ->
      let Just fuel = catalogIndex standardCatalog code
          computed   = spread fuel env
          computedAz = spreadAtAzimuth computed az
          (expected, expectedAz)   = standardSpread code env az
      in --traceShow (computedAz, expectedAz)
         (computed `spreadEq` expected) && (computedAz `spreadAzEq` expectedAz)

spreadEq :: Spread -> Spread -> Bool
spreadEq a b = all id [
    spreadRxInt        a `almostEq` spreadRxInt        b
  , spreadSpeed0       a `almostEq` spreadSpeed0       b
  , spreadHpua         a `almostEq` spreadHpua         b
  , spreadPhiEffWind   a `almostEq` spreadPhiEffWind   b
  , spreadSpeedMax     a `almostEq` spreadSpeedMax     b
  , spreadAzimuthMax   a `almostEq` spreadAzimuthMax   b
  , spreadEccentricity a `almostEq` spreadEccentricity b
  , spreadByramsMax    a `almostEq` spreadByramsMax    b
  , spreadFlameMax     a `almostEq` spreadFlameMax     b
  ]

spreadAzEq :: SpreadAtAzimuth -> SpreadAtAzimuth -> Bool
spreadAzEq a b = all id [
    spreadSpeed  a `almostEq` spreadSpeed   b
  , spreadByrams a `almostEq` spreadByrams  b
  , spreadFlame  a `almostEq` spreadFlame   b
  ]

almostEq :: Double -> Double -> Bool
almostEq a' b' = abs (a'-b') < tolerance
  where tolerance = 1e-6

newtype FuelCode = FuelCode Int deriving (Eq, Show)
newtype Env = Env SpreadEnv deriving (Eq, Show)
newtype Azimuth = Azimuth Double deriving (Eq, Show)

instance Arbitrary FuelCode where
  arbitrary = FuelCode <$> choose (0,13)

instance Arbitrary Azimuth where
  arbitrary = Azimuth <$> choose (0,359)

instance Arbitrary Env where
  arbitrary = Env <$> spreadEnv
    where spreadEnv = SpreadEnv <$> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1)
                                <*> choose (0,1000)
                                <*> choose (0,359)
                                <*> choose (0,1)
                                <*> choose (0,359)
