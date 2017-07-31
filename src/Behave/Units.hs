{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Behave.Units (
    module Export
  , DMoisture
  , DFraction
  , DRatio
  , DAzimuth

  , FuelLoad
  , SaToVolRatio
  , HeatOfCombustion
  , HeatPerUnitArea
  , ReactionVelocity
  , Ratio
  , Length
  , Dimensionless
  , Fraction
  , Moisture
  , TotalMineralContent
  , EffectiveMineralContent
  , Speed
  , ByramsIntensity
  , RateOfSpread
  , Azimuth
  , ReactionIntensity
  , Time
  , lbSqFt
  , lbCuFt
  , btu
  , btuLb
  , btuFtSec
  , btuSqFtMin
  , btuSqFt
  , perFoot
  , footMin
  , one
  , perCent
  , perOne
  , _0
  , _1
  , (*~)
  , (/~)
) where

import           Numeric.Units.Dimensional
import           Numeric.Units.Dimensional.UnitNames (atom)
import           Numeric.Units.Dimensional.Prelude
import           Numeric.Units.Dimensional.NonSI as Export hiding (btu)
import           Numeric.Units.Dimensional.SIUnits as Export
import           Numeric.Units.Dimensional.Quantities as Export
import           Numeric.NumType.DK.Integers (TypeInt(..))
import           Prelude () -- for instances

{-
    1. Lóngitud
    2. Masa
    3. Tiempo
    4. Corriente eléctrica
    5. Temperatura termodinámica
    6. Cantidad de substancia
    7. Intensidad lumínica
-}
type DFuelLoad              = 'Dim 'Neg2 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type DSaToVolRatio          = 'Dim 'Neg1 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type DHeatOfCombustion      = 'Dim 'Pos2 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
type DHeatPerUnitArea       = 'Dim 'Zero 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
type DReactionVelocity      = 'Dim 'Zero 'Zero 'Neg1 'Zero 'Zero 'Zero 'Zero
type DByramsIntensity       = 'Dim 'Pos1 'Pos1 'Neg3 'Zero 'Zero 'Zero 'Zero
type DRatio                 = DOne
type DFraction              = DOne
type DMoisture              = DFraction
type DAzimuth               = DPlaneAngle

type FuelLoad                = Quantity DFuelLoad Double
type SaToVolRatio            = Quantity DSaToVolRatio Double
type HeatOfCombustion        = Quantity DHeatOfCombustion Double
type HeatPerUnitArea         = Quantity DHeatPerUnitArea Double
type ReactionVelocity        = Quantity DReactionVelocity Double
type ByramsIntensity         = Quantity DByramsIntensity Double
type Ratio                   = Quantity DRatio Double
type Fraction                = Quantity DFraction Double
type Moisture                = Quantity DMoisture Double
type TotalMineralContent     = Fraction
type EffectiveMineralContent = Fraction
type Speed                   = Quantity DVelocity Double
type RateOfSpread            = Speed
type Azimuth                 = Quantity DAzimuth Double
type ReactionIntensity       = HeatFluxDensity Double

perCent :: Fractional a => Unit 'NonMetric DOne a
perCent = mkUnitQ n 0.01 one
  where n = atom "[%]" "%" "Per cent"

perOne :: Fractional a => Unit 'NonMetric DOne a
perOne = mkUnitQ n 1.0 one
  where n = atom "[one]" "one" "Ratio"

btu :: Fractional a => Unit 'NonMetric DEnergy a
btu = mkUnitQ n 0.293071 (watt * hour)
  where n = atom "[btu]" "btu" "British Thermal Unit"


lbSqFt :: Unit 'NonMetric DFuelLoad Double
lbSqFt = poundMass/(foot ^ pos2)

lbCuFt :: Unit 'NonMetric DDensity Double
lbCuFt = poundMass/(foot ^ pos3)

btuLb:: Unit 'NonMetric DHeatOfCombustion Double
btuLb = btu / poundMass

btuFtSec :: Unit 'NonMetric DByramsIntensity Double
btuFtSec = btu / foot / second

btuSqFtMin :: Unit 'NonMetric DHeatFluxDensity Double
btuSqFtMin = btuSqFt / minute

btuSqFt :: Unit 'NonMetric DHeatPerUnitArea Double
btuSqFt = btu / foot ^ pos2

perFoot :: Unit 'NonMetric DSaToVolRatio Double
perFoot  = foot ^ neg1

footMin :: Unit 'NonMetric DVelocity Double
footMin = foot / minute
