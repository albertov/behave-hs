{-# LANGUAGE RecordWildCards #-}
module Behave (
    Catalog
  , Particle (..)
  , Fuel (..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , Combustion (..)
  , spread
  , spreadAtAzimuth
  , standardCatalog
  , indexCatalog
  , mkCatalog
) where

import qualified Data.Vector.Unboxed as U
import Behave.Types

-- | Calculates fire spread paramaters
spread :: Fuel -> SpreadEnv -> Spread
spread fuel@Fuel{fuelParticles=particles,..} env@SpreadEnv{..}
  | U.null particles = noSpread
  | otherwise        = Spread {
      spreadRxInt        = rxInt
    , spreadSpeed0       = speed0
    , spreadHpua         = hpua
    , spreadPhiEffWind   = phiEffWind
    , spreadSpeedMax     = speedMax
    , spreadAzimuthMax   = azimuthMax
    , spreadEccentricity = eccentricity
    , spreadByramsMax    = byrams
    , spreadFlameMax     = flame
    }
  where
    Combustion{..}  = fuelCombustion fuel

    wfmd            = accumBy (\p -> partMoisture p env
                                   * partSigmaFactor p
                                   * partLoad p)
                    $ lifeParticles Dead particles

    lifeMext Dead   = fuelMext

    lifeMext Alive
      | fuelHasLiveParticles fuel = max liveMext fuelMext
      | otherwise                 = 0
      where liveMext = (combLiveExtFactor * (1 - fdmois/fuelMext)) - 0.226
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

    phiSlope        = combSlopeK * (envSlope ^! 2)

    phiWind
      | envWindSpeed < smidgen = 0
      | otherwise              = combWindK * (envWindSpeed ** combWindB)

    phiEw = phiWind + phiSlope

    eccentricity
      | effWind > smidgen
      , lwRatio > (1+smidgen) = sqrt (lwRatio * lwRatio - 1) / lwRatio
      | otherwise             = 0
      where lwRatio = 1 + 0.002840909 * effWind

    byrams = combResidenceTime * speedMax * rxInt / 60

    flame  = flameLength byrams

    (phiEffWind,effWind,speedMax,azimuthMax)
      = case situation of
          NoSpread ->
            (phiEw, 0, 0, 0)
          NoSlopeNoWind ->
            (phiEw, 0, speed0, 0)
          WindNoSlope ->
            checkWindLimit envWindSpeed phiEw speedMax' envWindAzimuth
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
                  | upslope <= envWindAzimuth = envWindAzimuth - upslope
                  | otherwise                 = 360 - upslope + envWindAzimuth
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
      | envSlope                    < smidgen = WindNoSlope
      | envWindSpeed                < smidgen = SlopeNoWind
      | abs(upslope-envWindAzimuth) < smidgen = UpSlope
      | otherwise                             = CrossSlope

    upslope
      | envAspect >= 180 = envAspect - 180
      | otherwise        = envAspect + 180

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
spreadAtAzimuth :: Spread -> Double -> SpreadAtAzimuth
spreadAtAzimuth Spread{..} azimuth
  = SpreadAtAzimuth {
      spreadSpeed  = spreadSpeedMax  * factor
    , spreadByrams = spreadByramsMax * factor
    , spreadFlame  = flameLength (spreadByramsMax * factor)
    }
  where
    factor
      | abs (azimuth - spreadAzimuthMax) < smidgen = 1
      | otherwise  = (1 - spreadEccentricity) `safeDiv`
                     (1 - spreadEccentricity * cos (degToRad angle))
    angle
      | ret > 180 = 360 - ret
      | otherwise = ret
      where ret = abs (spreadAzimuthMax - azimuth)


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
                                        * partLoad p
                                        * (1 - partSiTotal p))
    lifeSavr        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partSavr p)
    lifeHeat        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partHeat p)
    lifeSeff        = accumByLife' (\p -> partAreaWtg fuel p
                                        * partSiEffective p)
    fuelBulkDensity = accumBy' partLoad `safeDiv` fuelDepth fuel
    beta            = accumBy' (\p -> partLoad p `safeDiv` partDensity p)
                    `safeDiv` fuelDepth fuel
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
    fineLive        = accumBy (\p -> partLoad p * exp ((-500) / partSavr p))
                    $ lifeParticles Alive particles
    fineDead        = accumBy (\p -> partLoad p * partSigmaFactor p)
                    $ lifeParticles Dead particles
    liveMextFactor  = 2.9 * fineDead `safeDiv` fineLive
    accumByLife'    = accumByLife particles
    accumBy' f      = accumBy f particles


partLife :: Particle -> Life
partLife Particle{..} = case partType of {ParticleDead -> Dead; _ -> Alive}

partSurfaceArea :: Particle -> Double
partSurfaceArea Particle{..}
  = partLoad * partSavr `safeDiv` partDensity

partSigmaFactor :: Particle -> Double
partSigmaFactor Particle {..}
  = exp ((-138) `safeDiv` partSavr)

partSizeClass :: Particle -> SizeClass
partSizeClass Particle{..}
  = fst . head . dropWhile ((>=partSavr) . snd) $ sizeBoundaries
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
partMoisture p
  = case partType p of
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

flameLength :: Double -> Double
flameLength byrams
  | byrams < smidgen = 0
  | otherwise        = 0.45 * (byrams ** 0.46)

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
