{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Behave (
    Particle (..)
  , Fuel (..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , spread
  , spread2
  , mkSpread
  , spreadAtAzimuth
  , prepareCatalog
  , module Behave.Types
) where

import qualified Data.Vector.Unboxed as U
import Behave.Types
import Behave.Units
import Numeric.Units.Dimensional.Functor ()
import Numeric.Units.Dimensional.Coercion (unQuantity)
import Unsafe.Coerce (unsafeCoerce)


mkSpread :: Catalog Fuel -> Int -> Maybe SpreadFunc
mkSpread catalog = indexCatalog (mapCatalog spread catalog)

prepareCatalog :: Catalog Fuel -> Catalog PreparedFuel
prepareCatalog = mapCatalog (\f -> (f, fuelCombustion f))

-- | Calculates fire spread paramaters
spread2 :: PreparedFuel -> SpreadEnv -> Spread
spread2 (fuel,combustion) = spread' fuel combustion

-- | Calculates fire spread paramaters
spread :: Fuel -> SpreadEnv -> Spread
spread fuel = spread' fuel (fuelCombustion fuel)

spread' :: Fuel -> Combustion -> SpreadEnv -> Spread
spread' fuel@(fuelParticles -> particles) Combustion{..} env
  | U.null particles = noSpread
  | otherwise        = Spread {
      _sRxInt        = rxInt        *~ btuSqFtMin
    , _sSpeed0       = speed0       *~ footMin
    , _sHpua         = hpua         *~ btuSqFt
    , _sPhiEffWind   = phiEffWind   *~ one
    , _sSpeedMax     = speedMax     *~ footMin
    , _sAzimuthMax   = unsafeCoerce azimuthMax
    , _sEccentricity = eccentricity *~ one
    , _sByramsMax    = byrams
    , _sFlameMax     = flame
    }
  where
    windSpeed   = _seWindSpeed env    /~ footMin
    windAzimuth = unQuantity (_seWindAzimuth env)
    slope       = _seSlope env        /~ one
    aspect      = unQuantity (_seAspect env)
    residenceTime = combResidenceTime /~ minute

    wfmd            = accumBy (\p -> partMoisture p env
                                   * partSigmaFactor p
                                   * partLoad' p)
                    $ fuelDeadParticles fuel

    lifeMext Dead   = fuelMext' fuel

    lifeMext Alive
      | fuelHasLiveParticles fuel = max liveMext (fuelMext' fuel)
      | otherwise                 = 0
      where liveMext = (combLiveExtFactor * (1 - fdmois/fuelMext' fuel)) - 0.226
            fdmois   = wfmd `safeDiv` combFineDeadFactor

    lifeMoisture    = accumByLife' (\p -> partAreaWtg fuel p
                                        * partMoisture p env)

    rbQig           = accumBy' f * combFuelBedBulkDens
      where f p = (250 + 1116 * partMoisture p env)
                * partAreaWtg fuel p
                * combLifeAreaWtg (partLife p)
                * partSigmaFactor p

    combLifeAreaWtg Alive = combLiveAreaWtg
    combLifeAreaWtg Dead  = combDeadAreaWtg

    combLifeRxFactor Alive = combLiveRxFactor
    combLifeRxFactor Dead  = combDeadRxFactor

    lifeEtaM lf
      | lifeMoisture lf >= lifeMext lf = 0
      | lifeMext lf > smidgen          = 1
                                       - 2.59 * rt^!1
                                       + 5.11 * rt^!2
                                       - 3.52 * rt^!3
      | otherwise                      = 0
      where rt = (lifeMoisture lf / lifeMext lf)

    rxInt           = combLifeRxFactor Alive * lifeEtaM Alive
                    + combLifeRxFactor Dead  * lifeEtaM Dead

    hpua            = rxInt * residenceTime

    speed0          = rxInt * combFluxRatio `safeDiv` rbQig

    phiSlope        = combSlopeK * (slope ^! 2)

    phiWind
      | windSpeed < smidgen = 0
      | otherwise           = combWindK * (windSpeed ** combWindB)

    phiEw = phiWind + phiSlope

    eccentricity
      | effWind > smidgen
      , lwRatio > (1+smidgen) = sqrt (lwRatio * lwRatio - 1) / lwRatio
      | otherwise             = 0
      where lwRatio = 1 + 0.002840909 * effWind

    byrams = (residenceTime * speedMax * rxInt / 60) *~ btuFtSec

    flame  = flameLength byrams

    (phiEffWind,effWind,speedMax,azimuthMax)
      = case situation of
          NoSpread ->
            (phiEw, 0, 0, 0)
          NoSlopeNoWind ->
            (phiEw, 0, speed0, 0)
          WindNoSlope ->
            checkWindLimit windSpeed phiEw speedMax' windAzimuth
          SlopeNoWind ->
            checkWindLimit (ewFromPhiEw phiEw) phiEw speedMax' upslope
          UpSlope ->
            checkWindLimit (ewFromPhiEw phiEw) phiEw speedMax' upslope
          CrossSlope ->
            let rv      = sqrt (x*x + y*y)
                x       = slpRate + wndRate * cos split
                y       = wndRate * sin split
                wndRate = speed0 * phiWind
                slpRate = speed0 * phiSlope
                split
                  | upslope <= windAzimuth = windAzimuth - upslope
                  | otherwise                 = 2*pi - upslope + windAzimuth
                speedMax'' = speed0 + rv
                phiEw'' = speedMax'' / speed0 - 1
                effWind'
                  | phiEw'' > smidgen = ewFromPhiEw phiEw''
                  | otherwise         = 0
                al      = asin (abs y / rv)
                split'
                  | x >= 0, y >= 0 = al
                  | x >= 0         = 2*pi - al
                  | y >= 0         = pi - al
                  | otherwise      = pi + al
                azimuthMax'
                  | ret > 2*pi = ret - 2*pi
                  | otherwise = ret
                  where ret = upslope + split'
            in checkWindLimit effWind' phiEw'' speedMax'' azimuthMax'
        where
          speedMax' = speed0 * (1 + phiEw)
          maxWind = 0.9 * rxInt
          ewFromPhiEw p  = (p * combWindE) ** (1 / combWindB)
          checkWindLimit ew pew s a
            | ew > maxWind = (phiEwMaxWind, maxWind, speedMaxWind, a)
            | otherwise    = (pew, ew, s, a)
            where
              speedMaxWind = speed0 * (1 + phiEwMaxWind)
              phiEwMaxWind
                | maxWind < smidgen = 0
                | otherwise         = combWindK * (maxWind ** combWindB)

    situation
      | speed0                      < smidgen = NoSpread
      | phiEw                       < smidgen = NoSlopeNoWind
      | slope                       < smidgen = WindNoSlope
      | windSpeed                   < smidgen = SlopeNoWind
      | abs(upslope-windAzimuth)    < smidgen = UpSlope
      | otherwise                             = CrossSlope

    upslope
      | aspect    >= pi = aspect - pi
      | otherwise       = aspect + pi

    accumByLife'    = accumByLife fuel
    accumBy' f      = accumBy f particles

data WindSlopeSituation
  = NoSpread
  | NoSlopeNoWind
  | WindNoSlope
  | SlopeNoWind
  | UpSlope
  | CrossSlope

-- | Calculates fire spread paramateres at a given azimuth
spreadAtAzimuth :: Spread -> Azimuth -> SpreadAtAzimuth
spreadAtAzimuth Spread{..} az
  = SpreadAtAzimuth {
      _sSpeed  = fmap (*factor) _sSpeedMax
    , _sByrams = fmap (*factor) _sByramsMax
    , _sFlame  = flameLength $ (fmap (*factor) _sByramsMax)
    }
  where
    azimuth    = unQuantity az
    azimuthMax = unQuantity _sAzimuthMax
    ecc        = _sEccentricity /~ one
    factor
      | abs (azimuth - azimuthMax) < smidgen = 1
      | otherwise  = (1 - ecc) `safeDiv` (1 - ecc * cos angle)
    angle
      | ret > pi = 2*pi - ret
      | otherwise = ret
      where ret = abs (azimuthMax - azimuth)


-- | Calculates fuel-dependent parameters
fuelCombustion :: Fuel -> Combustion
fuelCombustion fuel@(fuelParticles -> particles)
  = Combustion {
      combLiveAreaWtg     = lifeAreaWtg Alive
    , combLiveRxFactor    = lifeRxFactor Alive
    , combDeadAreaWtg     = lifeAreaWtg Dead
    , combDeadRxFactor    = lifeRxFactor Dead
    , combFineDeadFactor  = fineDead
    , combLiveExtFactor   = liveMextFactor
    , combFuelBedBulkDens = fuelBulkDensity
    , combResidenceTime   = residenceTime *~ minute
    , combFluxRatio       = fluxRatio
    , combSlopeK          = slopeK
    , combWindB           = windB
    , combWindE           = windE
    , combWindK           = windK
    }
  where
    lifeAreaWtg     = accumByLife' ((/totalArea) . partSurfaceArea)
    totalArea       = accumBy' partSurfaceArea
    lifeLoad        = accumByLife' (\p -> partSizeClassWtg fuel p
                                        * partLoad' p
                                        * (1 - partSiTotal' p))
    lifeSavr        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partSavr' p)
    lifeHeat        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partHeat' p)
    lifeSeff        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partSiEffective' p)
    fuelBulkDensity = accumBy' partLoad' `safeDiv` fuelDepth' fuel
    beta            = accumBy' (\p -> partLoad' p `safeDiv` partDensity' p)
                    `safeDiv` fuelDepth' fuel
    sigma           = lifeAreaWtg Alive * lifeSavr Alive
                    + lifeAreaWtg Dead  * lifeSavr Dead
    lifeRxFactor lf = lifeLoad lf * lifeHeat lf * lifeEtaS lf * gamma
    gamma           = gammaMax * (ratio ** aa) * exp (aa * (1-ratio))
      where
        gammaMax = sigma15 / (495 + 0.0594*sigma15)
        sigma15  = sigma ** 1.5
        aa       = 133 / (sigma ** 0.7913)
    ratio           = beta / (3.348 / (sigma ** 0.8189))
    lifeEtaS lf
      | lifeSeff lf <= smidgen = 1
      | eta < 1                = eta
      | otherwise              = 1
      where eta = 0.174 / lifeSeff lf ** 0.19
    residenceTime   = 384 / sigma
    fluxRatio       = exp ((0.792 + 0.681 * sqrt sigma) * (beta + 0.1))
                    / (192 + 0.2595*sigma)
    slopeK          = 5.275 * (beta ** (-0.3))
    windB           = 0.02526 * (sigma ** 0.54)
    (windK, windE)  = (c * (ratio ** (-e)), (ratio ** e) / c)
      where c = 7.47  * exp ((-0.133) * (sigma ** 0.55))
            e = 0.715 * exp ((-0.000359) * sigma)
    fineLive        = accumBy (\p -> partLoad' p * exp ((-500) / partSavr' p))
                    $ fuelAliveParticles fuel
    fineDead        = accumBy (\p -> partLoad' p * partSigmaFactor p)
                    $ fuelDeadParticles fuel
    liveMextFactor  = 2.9 * fineDead `safeDiv` fineLive
    accumByLife'    = accumByLife fuel
    accumBy' f      = accumBy f particles

fuelDepth' :: Fuel -> Double
fuelDepth' = (/~foot) . fuelDepth

fuelMext' :: Fuel -> Double
fuelMext' = (/~one) . fuelMext

partDensity' :: Particle -> Double
partDensity' = (/~lbCuFt) . partDensity

partSiEffective' :: Particle -> Double
partSiEffective' = (/~one) . partSiEffective

partHeat' :: Particle -> Double
partHeat' = (/~btuLb) . partHeat

partSavr' :: Particle -> Double
partSavr' = (/~perFoot) . partSavr

partSiTotal' :: Particle -> Double
partSiTotal' = (/~one) . partSiTotal

partLoad' :: Particle -> Double
partLoad' = (/~lbSqFt) . partLoad

partSurfaceArea :: Particle -> Double
partSurfaceArea p
  = partLoad' p  * partSavr' p `safeDiv` partDensity' p

partSigmaFactor :: Particle -> Double
partSigmaFactor p
  = exp ((-138) `safeDiv` partSavr' p)

partSizeClass :: Particle -> SizeClass
partSizeClass p
  = fst . head . dropWhile ((>=partSavr' p) . snd) $ sizeBoundaries
  where
    sizeBoundaries = zip [SC0 ..] [1200, 192, 96, 48, 16, 0]

partAreaWtg :: Fuel -> Particle -> Double
partAreaWtg fuel part
  = partSurfaceArea part `safeDiv` accumBy partSurfaceArea sameLifeParticles
  where
    sameLifeParticles = fuelLifeParticles (partLife part) fuel

partSizeClassWtg :: Fuel -> Particle -> Double
partSizeClassWtg fuel part
 = accumBy (partAreaWtg fuel)
 . U.filter ((== partSizeClass part) . partSizeClass)
 $ fuelLifeParticles (partLife part) fuel

partMoisture :: Particle -> SpreadEnv -> Double
partMoisture p = (/~one) . go
  where
    go = case partType p of
            ParticleHerb -> _seHerb
            ParticleWood -> _seWood
            ParticleDead ->
              case partSizeClass p of
                SC0 -> _seD1hr
                SC1 -> _seD1hr
                SC2 -> _seD10hr
                SC3 -> _seD10hr
                SC4 -> _seD100hr
                SC5 -> _seD100hr

fuelHasLiveParticles :: Fuel -> Bool
fuelHasLiveParticles = not . U.null . fuelAliveParticles

flameLength :: ByramsIntensity -> Length Double
flameLength byrams'
  | byrams < smidgen = _0
  | otherwise        = (0.45 * (byrams ** 0.46)) *~ foot
  where byrams = byrams' /~ btuFtSec

--
-- Utilities
--
smidgen :: Fractional a => a
smidgen = 1e-6

-- | Avoids division by positive numbers close to zero
safeDiv :: (Ord a, Fractional a) => a -> a -> a
safeDiv a b
  | b > smidgen = a / b
  | otherwise   = 0
{-# INLINE safeDiv #-}

-- | Accumulates values over a Particle projection
accumBy :: (U.Unbox a, Num a) => (Particle -> a) -> U.Vector Particle -> a
accumBy f = U.sum . U.map f
{-# INLINE accumBy #-}

-- Accumulates values over a Particle projection indexed by their 'Life'
-- property
accumByLife
  :: (U.Unbox a, Num a)
  => Fuel -> (Particle -> a) -> (Life -> a)
accumByLife fuel f life = accumBy f (fuelLifeParticles life fuel)
{-# INLINE accumByLife #-}

-- | A monomorphic version of '^'
infixr 8 ^!
(^!) :: Fractional a => a -> Int -> a
(^!) = (^)
{-# INLINE (^!) #-}
