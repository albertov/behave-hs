module Arbitrary where

import Test.QuickCheck (Arbitrary(..), Gen, choose)
import Behave
import Behave.Units (Azimuth, (*~), one, footMin, degree)

newtype FuelCode = FuelCode Int deriving (Eq, Show)
newtype Env = Env SpreadEnv deriving (Eq, Show)
newtype ArbAzimuth = ArbAzimuth Azimuth deriving (Eq, Show)

instance Arbitrary FuelCode where
  arbitrary = FuelCode <$> choose (0,13)

instance Arbitrary ArbAzimuth where
  arbitrary = ArbAzimuth <$> bearing

bearing :: Gen Azimuth
bearing  = fmap (*~degree) (choose (0,359))

instance Arbitrary Env where
  arbitrary = Env <$> spreadEnv
    where
      fraction = fmap (*~one) (choose (0,1))
      speed    = fmap (*~footMin) (choose (0,100))
      spreadEnv = SpreadEnv <$> fraction
                            <*> fraction
                            <*> fraction
                            <*> fraction
                            <*> fraction
                            <*> speed
                            <*> bearing
                            <*> fraction
                            <*> bearing

