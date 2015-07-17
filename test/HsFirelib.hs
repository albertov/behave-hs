{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module HsFirelib (standardSpread) where

import qualified Language.C.Inline as C (include)
import qualified Language.C.Inline.Unsafe as C
import           Foreign.C.Types
import           Foreign.Ptr
import           System.IO.Unsafe (unsafePerformIO)
import           Behave
import           Behave.Units 

C.include "fireLib.h"

newtype Catalog = Catalog (Ptr ())

standardCatalog :: IO Catalog
standardCatalog
  = fmap Catalog [C.exp| void *{Fire_FuelCatalogCreateStandard("standard", 20)}|]

initFuel :: Catalog -> Int -> SpreadEnv -> IO ()
initFuel (Catalog catalog) fuel SpreadEnv{..} = do
  let d1hr    = realToFrac (envD1hr   /~ one)
      d10hr   = realToFrac (envD10hr  /~ one)
      d100hr  = realToFrac (envD100hr /~ one)
      herb    = realToFrac (envHerb   /~ one)
      wood    = realToFrac (envWood   /~ one)
      model   = fromIntegral fuel
      windFpm = realToFrac (envWindSpeed   /~ footMin)
      windDeg = realToFrac (envWindAzimuth /~ degree)
      slope   = realToFrac (envSlope       /~ one)
      aspect  = realToFrac (envAspect      /~ degree)
  [C.block| void {
      double moisture[6] = { $(double d1hr)
                           , $(double d10hr)
                           , $(double d100hr)
                           , 0
                           , $(double herb)
                           , $(double wood)
                           };
      Fire_SpreadNoWindNoSlope($(void *catalog), $(size_t model), moisture);
      Fire_SpreadWindSlopeMax( $(void *catalog), $(size_t model)
                             , $(double windFpm), $(double windDeg)
                             , $(double slope), $(double aspect));
      double az = Fuel_AzimuthMax( (FuelCatalogPtr)$(void *catalog)
                                  , $(size_t model));
      size_t w = FIRE_BYRAMS | FIRE_FLAME;
      Fire_SpreadAtAzimuth( $(void *catalog), $(size_t model), az, w);
  } |]

setAzimuth :: Catalog -> Int -> Azimuth -> IO ()
setAzimuth (Catalog catalog) fuel azimuth = do
  let az    = realToFrac (azimuth /~ degree)
      model = fromIntegral fuel
  [C.block| void {
      size_t w = FIRE_BYRAMS | FIRE_FLAME;
      Fire_SpreadAtAzimuth( $(void *catalog), $(size_t model), $(double az), w);
  } |]

destroyCatalog :: Catalog -> IO ()
destroyCatalog (Catalog catalog)
  = [C.exp|void {Fire_FuelCatalogDestroy($(void *catalog))}|]

withCatalog :: (Catalog -> IO a) -> IO a
withCatalog f = do
  c <- standardCatalog
  ret <- f c
  destroyCatalog c
  return ret

standardSpread :: Int -> SpreadEnv -> Azimuth -> (Spread, SpreadAtAzimuth)
standardSpread fuel env azimuth = unsafePerformIO $ do
  let f = fromIntegral fuel
      d g = fmap (g . realToFrac)
  withCatalog $ \catalog@(Catalog c) -> do
    initFuel catalog fuel env
    spread <- Spread
      <$> d (*~btuSqFtMin) [C.exp|double {
             Fuel_RxIntensity((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~footMin) [C.exp|double {
             Fuel_Spread0((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~btuSqFt) [C.exp|double {
             Fuel_HeatPerUnitArea((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~one) [C.exp|double {
             Fuel_PhiEffWind((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~footMin) [C.exp|double {
             Fuel_SpreadMax((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~degree) [C.exp|double {
             Fuel_AzimuthMax((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~one) [C.exp|double {
             Fuel_Eccentricity((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~btuFtSec) [C.exp|double {
             Fuel_ByramsIntensity((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~foot) [C.exp|double {
             Fuel_FlameLength((FuelCatalogPtr)$(void *c), $(size_t f))}|]
    setAzimuth catalog fuel azimuth
    spreadAz <- SpreadAtAzimuth
      <$> d (*~footMin) [C.exp|double {
             Fuel_SpreadAny((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~btuFtSec) [C.exp|double {
             Fuel_ByramsIntensity((FuelCatalogPtr)$(void *c), $(size_t f))}|]
      <*> d (*~foot) [C.exp|double {
             Fuel_FlameLength((FuelCatalogPtr)$(void *c), $(size_t f))}|]
    return (spread, spreadAz)
