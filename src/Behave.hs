{-# LANGUAGE RecordWildCards #-}
module Behave (
    Catalog
  , Particle (..)
  , Fuel (..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , spread
  , mkSpread
  , spreadAtAzimuth
  , standardCatalog
  , indexCatalog
  , mkCatalog
) where

import qualified Data.Vector.Unboxed as U
import Behave.Types
import Behave.Units 
import Numeric.Units.Dimensional.DK.Functor ()

mkSpread :: Catalog Fuel -> Int -> Maybe (SpreadEnv -> Spread)
mkSpread catalog = indexCatalog (fmap spread catalog)

-- | Calculates fire spread paramaters
spread :: Fuel -> SpreadEnv -> Spread
spread fuel = spread' fuel (fuelCombustion fuel)

spread' :: Fuel -> Combustion -> SpreadEnv -> Spread
spread' fuel@Fuel{fuelParticles=particles} Combustion{..} env
  | U.null particles = noSpread
  | otherwise        = Spread {
      spreadRxInt        = rxInt        *~ btuSqFtMin
    , spreadSpeed0       = speed0       *~ footMin
    , spreadHpua         = hpua         *~ btuSqFt
    , spreadPhiEffWind   = phiEffWind   *~ one
    , spreadSpeedMax     = speedMax     *~ footMin
    , spreadAzimuthMax   = azimuthMax   *~ degree
    , spreadEccentricity = eccentricity *~ one
    , spreadByramsMax    = byrams
    , spreadFlameMax     = flame
    }
  where
    windSpeed   = envWindSpeed env   /~ footMin
    windAzimuth = envWindAzimuth env /~ degree
    slope       = envSlope env       /~ one
    aspect      = envAspect env      /~ degree

    wfmd            = accumBy (\p -> partMoisture p env
                                   * partSigmaFactor p
                                   * partLoad' p)
                    $ lifeParticles Dead particles

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

    hpua            = rxInt * combResidenceTime

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

    byrams = (combResidenceTime * speedMax * rxInt / 60) *~ btuFtSec

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
                x       = slpRate + wndRate * cos (degToRad split)
                y       = wndRate * sin (degToRad split)
                wndRate = speed0 * phiWind
                slpRate = speed0 * phiSlope
                split
                  | upslope <= windAzimuth = windAzimuth - upslope
                  | otherwise                 = 360 - upslope + windAzimuth
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
                  | ret > 360 = ret - 360
                  | otherwise = ret
                  where ret = upslope + radToDeg split'
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
      | aspect    >= 180 = aspect - 180
      | otherwise        = aspect + 180

    accumByLife'    = accumByLife particles
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
      spreadSpeed  = fmap (*factor) spreadSpeedMax
    , spreadByrams = fmap (*factor) spreadByramsMax
    , spreadFlame  = flameLength $ (fmap (*factor) spreadByramsMax)
    }
  where
    azimuth    = az /~ degree
    azimuthMax = spreadAzimuthMax /~ degree
    ecc        = spreadEccentricity /~ one
    factor
      | abs (azimuth - azimuthMax) < smidgen = 1
      | otherwise  = (1 - ecc) `safeDiv` (1 - ecc * cos (degToRad angle))
    angle
      | ret > 180 = 360 - ret
      | otherwise = ret
      where ret = abs (azimuthMax - azimuth)


-- | Calculates fuel-dependent parameters
fuelCombustion :: Fuel -> Combustion
fuelCombustion fuel@Fuel{fuelParticles=particles}
  = Combustion {
      combLifeAreaWtg     = lifeAreaWtg
    , combLifeRxFactor    = lifeRxFactor
    , combFineDeadFactor  = fineDead
    , combLiveExtFactor   = liveMextFactor
    , combFuelBedBulkDens = fuelBulkDensity
    , combResidenceTime   = residenceTime
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
                    $ lifeParticles Alive particles
    fineDead        = accumBy (\p -> partLoad' p * partSigmaFactor p)
                    $ lifeParticles Dead particles
    liveMextFactor  = 2.9 * fineDead `safeDiv` fineLive
    accumByLife'    = accumByLife particles
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

partLife :: Particle -> Life
partLife Particle{..} = case partType of {ParticleDead -> Dead; _ -> Alive}

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
    sameLifeParticles = lifeParticles (partLife part) (fuelParticles fuel)

partSizeClassWtg :: Fuel -> Particle -> Double
partSizeClassWtg fuel part
 = accumBy (partAreaWtg fuel)
 . U.filter ((== partSizeClass part) . partSizeClass)
 $ lifeParticles (partLife part) (fuelParticles fuel)

partMoisture :: Particle -> SpreadEnv -> Double
partMoisture p = (/~one) . go
  where
    go = case partType p of
            ParticleHerb -> envHerb
            ParticleWood -> envWood
            ParticleDead ->
              case partSizeClass p of
                SC0 -> envD1hr
                SC1 -> envD1hr
                SC2 -> envD10hr
                SC3 -> envD10hr
                SC4 -> envD100hr
                SC5 -> envD100hr

fuelHasLiveParticles :: Fuel -> Bool
fuelHasLiveParticles
  = not . U.null . U.dropWhile (==ParticleDead) . U.map partType . fuelParticles

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

-- | Accumulates values over a Particle projection
accumBy :: (U.Unbox a, Num a) => (Particle -> a) -> U.Vector Particle -> a
accumBy f = U.sum . U.map f

-- Accumulates values over a Particle projection indexed by their 'Life'
-- property
accumByLife
  :: (U.Unbox a, Num a)
  => U.Vector Particle -> (Particle -> a) -> (Life -> a)
accumByLife ps f life = accumBy f (lifeParticles life ps)

-- | Filters particles by the 'Life' property
lifeParticles :: Life -> U.Vector Particle -> U.Vector Particle
lifeParticles life = U.filter ((== life) . partLife)

degToRad :: Double -> Double
degToRad = (*) 0.017453293

radToDeg :: Double -> Double
radToDeg = (*) 57.29577951

-- | A monomorphic version of '^'
infixr 8 ^!
(^!) :: Fractional a => a -> Int -> a
(^!) = (^)
