{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , Moisture
  , TotalMineralContent
  , EffectiveMineralContent
  , Speed
  , RateOfSpread
  , Azimuth
  , Slope
  , ReactionIntensity
  , lbSqFt
  , lbCuFt
  , btu
  , btuLb
  , perFoot
  , toQ
  , unQ
  , dl
) where

import           Numeric.Units.Dimensional.DK as Export
import           Numeric.Units.Dimensional.DK.Prelude as Export
import           Numeric.Units.Dimensional.DK.NonSI as Export
import           Numeric.NumType.DK.Integers (TypeInt(..))
import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

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

type FuelLoad                = Quantity DFuelLoad Double
type SaToVolRatio            = Quantity DSaToVolRatio Double
type HeatOfCombustion        = Quantity DHeatOfCombustion Double
type HeatPerUnitArea         = Quantity DHeatPerUnitArea Double
type ReactionVelocity        = Quantity DReactionVelocity Double
type Ratio                   = Dimensionless Double
type Moisture                = Ratio
type TotalMineralContent     = Ratio
type EffectiveMineralContent = Ratio
type Speed                   = Velocity Double
type RateOfSpread            = Speed
type Azimuth                 = PlaneAngle Double
type Slope                   = Ratio
type ReactionIntensity       = HeatFluxDensity Double

lbSqFt :: Unit DFuelLoad Double
lbSqFt = poundMass/(foot ^ pos2)

lbCuFt :: Unit DDensity Double
lbCuFt = poundMass/(foot ^ pos3)

btu:: Unit DEnergy Double
btu = prefix 0.293071 (watt * hour)

btuLb:: Unit DHeatOfCombustion Double
btuLb = btu / poundMass

perFoot :: Unit DSaToVolRatio Double
perFoot  = foot ^ neg1

toQ :: a -> Quantity d a
toQ = unsafeCoerce

unQ :: Quantity d a -> a
unQ = unsafeCoerce

derivingUnbox "Quantity"
    [t| forall d a. U.Unbox a => Quantity d a -> a |]
    [|unQ|]
    [|toQ|]

dl :: Num a => a -> Dimensionless a
dl a = a *~ one
{-# INLINE dl #-}
