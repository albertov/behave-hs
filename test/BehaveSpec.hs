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
    _sRxInt        a `almostEq` _sRxInt        b
  , _sSpeed0       a `almostEq` _sSpeed0       b
  , _sHpua         a `almostEq` _sHpua         b
  , _sPhiEffWind   a `almostEq` _sPhiEffWind   b
  , _sSpeedMax     a `almostEq` _sSpeedMax     b
  , _sAzimuthMax   a `almostEq` _sAzimuthMax   b
  , _sEccentricity a `almostEq` _sEccentricity b
  , _sByramsMax    a `almostEq` _sByramsMax    b
  , _sFlameMax     a `almostEq` _sFlameMax     b
  ]

spreadAzEq :: SpreadAtAzimuth -> SpreadAtAzimuth -> Bool
spreadAzEq a b = all id [
    _sSpeed  a `almostEq` _sSpeed   b
  , _sByrams a `almostEq` _sByrams  b
  , _sFlame  a `almostEq` _sFlame   b
  ]

almostEq :: Quantity d Double -> Quantity d Double -> Bool
almostEq a b = abs (a'-b') < tolerance
  where tolerance, a', b' :: Double
        tolerance = 1e-6
        a' = unsafeCoerce a
        b' = unsafeCoerce b
