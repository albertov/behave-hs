{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BehaveSpec (main, spec) where

import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import HsFirelib (standardSpread)
import Debug.Trace
import Behave
import Numeric.Units.Dimensional (Quantity)
import Unsafe.Coerce (unsafeCoerce)
import Arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spread" $ do
    prop "behaves like fireLib" $ \(FuelCode code, Env env, ArbAzimuth az) ->
      let Just fuel = indexCatalog standardCatalog code
          computed   = spread fuel env
          computedAz = spreadAtAzimuth computed az
          (expected, expectedAz)   = standardSpread code env az
      in traceShow (computedAz, expectedAz)
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

almostEq :: Quantity d Double -> Quantity d Double -> Bool
almostEq a b = abs (a'-b') < tolerance
  where tolerance, a', b' :: Double
        tolerance = 1e-6
        a' = unsafeCoerce a
        b' = unsafeCoerce b
