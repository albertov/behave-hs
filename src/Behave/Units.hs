{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Behave.Units (
    module Export
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
  , _0
  , _1
  , (*~)
  , (/~)
) where

import           Numeric.Units.Dimensional.DK
import           Numeric.Units.Dimensional.DK.Vector ()
import           Numeric.Units.Dimensional.DK.Prelude
import           Numeric.Units.Dimensional.DK.NonSI as Export
import           Numeric.Units.Dimensional.DK.SIUnits as Export
import           Numeric.Units.Dimensional.DK.Quantities as Export
import           Numeric.NumType.DK.Integers (TypeInt(..))
import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector.Unboxed as U

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

type FuelLoad                = Quantity DFuelLoad Double
type SaToVolRatio            = Quantity DSaToVolRatio Double
type HeatOfCombustion        = Quantity DHeatOfCombustion Double
type HeatPerUnitArea         = Quantity DHeatPerUnitArea Double
type ReactionVelocity        = Quantity DReactionVelocity Double
type ByramsIntensity         = Quantity DByramsIntensity Double
type Ratio                   = Dimensionless Double
type Fraction                = Dimensionless Double
type Moisture                = Fraction
type TotalMineralContent     = Fraction
type EffectiveMineralContent = Fraction
type Speed                   = Velocity Double
type RateOfSpread            = Speed
type Azimuth                 = PlaneAngle Double
type ReactionIntensity       = HeatFluxDensity Double

lbSqFt :: Unit DFuelLoad Double
lbSqFt = poundMass/(foot ^ pos2)

lbCuFt :: Unit DDensity Double
lbCuFt = poundMass/(foot ^ pos3)

btu:: Unit DEnergy Double
btu = prefix 0.293071 (watt * hour)

btuLb:: Unit DHeatOfCombustion Double
btuLb = btu / poundMass

btuFtSec :: Unit DByramsIntensity Double
btuFtSec = btu / foot / second

btuSqFtMin :: Unit DHeatFluxDensity Double
btuSqFtMin = btuSqFt / minute

btuSqFt :: Unit DHeatPerUnitArea Double
btuSqFt = btu / foot ^ pos2 

perFoot :: Unit DSaToVolRatio Double
perFoot  = foot ^ neg1

footMin :: Unit DVelocity Double
footMin = foot / minute
