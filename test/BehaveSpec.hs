{-# LANGUAGE RecordWildCards #-}
module BehaveSpec (main, spec) where

import Test.Hspec (Spec, hspec, describe, shouldBe, it, expectationFailure)
import Behave

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
