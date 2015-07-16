{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Behave (
    Catalog(..)
  , Particle(..)
  , Fuel(..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , spread
  , spreadAtAzimuth
  , standardCatalog
  , catalogIndex
) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Text (Text)
import           Behave.Units
import qualified Prelude as P

smidgen :: Fractional a => a
smidgen = 1e-6

safeDiv
  :: (Ord a, Fractional a)
  => Quantity d1 a -> Quantity d2 a -> Quantity (d1/d2) a
safeDiv a b
  | b > toQ smidgen = a / b
  | otherwise       = toQ 0

data Particle
  = Particle {
      partType        :: !ParticleType
    , partLoad        :: !FuelLoad
    , partSavr        :: !SaToVolRatio
    , partDensity     :: !(Density Double)
    , partHeat        :: !HeatOfCombustion
    , partSiTotal     :: !TotalMineralContent
    , partSiEffective :: !EffectiveMineralContent
  } deriving (Eq, Show)

partLife :: Particle -> Life
partLife Particle{..} = case partType of {ParticleDead -> Dead; _ -> Alive}

partSurfaceArea :: Particle -> Ratio
partSurfaceArea Particle{..}
  = partLoad * partSavr `safeDiv` partDensity

partSigmaFactor :: Particle -> Ratio
partSigmaFactor Particle {..}
  = exp (((-138) *~ perFoot) `safeDiv` partSavr)


partSizeClass :: Particle -> SizeClass
partSizeClass Particle{..}
  = fst . head . dropWhile ((>=partSavr) . snd) $ sizeBoundaries
  
data Life = Dead | Alive deriving (Eq, Show, Enum)

data SizeClass = SC0 | SC1 | SC2 | SC3 | SC4 | SC5
  deriving (Eq, Show, Enum)

sizeBoundaries :: [(SizeClass, SaToVolRatio)]
sizeBoundaries = zip [SC0 ..] [ 1200 *~ perFoot
                              , 192  *~ perFoot
                              , 96   *~ perFoot
                              , 48   *~ perFoot
                              , 16   *~ perFoot
                              , 0    *~ perFoot
                              ]


data ParticleType
  = ParticleDead
  | ParticleHerb
  | ParticleWood
  deriving (Show, Eq, Enum)

derivingUnbox "ParticleType"
    [t| ParticleType -> Int |]
    [|fromEnum|]
    [|toEnum|]

derivingUnbox "Particle"
    [t| Particle -> ( (ParticleType,FuelLoad,SaToVolRatio)
                    , ( Density Double,HeatOfCombustion
                      , TotalMineralContent,EffectiveMineralContent)) |]
    [| \(Particle a b c d e f g) -> ((a,b,c),(d,e,f,g)) |]
    [| \((a,b,c),(d,e,f,g)) -> Particle a b c d e f g|]


data Fuel
  = Fuel {
      fuelName      :: !Text                -- ^ name
    , fuelDesc      :: !Text                -- ^ description
    , fuelDepth     :: !(Length Double)     -- ^ total depth
    , fuelMext      :: !Moisture            -- ^ moisture of extinction
    , fuelAdjust    :: !Ratio               -- ^ adjustment factor
    , fuelParticles :: !(U.Vector Particle) -- ^ particle array
  } deriving (Eq, Show)

lifeParticles :: Life -> U.Vector Particle -> U.Vector Particle
lifeParticles life = U.filter ((== life) . partLife)

accumBy
  :: (U.Unbox a, U.Unbox (Quantity d b), Num b)
  => (a -> Quantity d b) -> U.Vector a -> Quantity d b
accumBy f = U.foldl' (+) (toQ 0) . U.map f

accumByLife
  :: (U.Unbox (Quantity d a), Num a)
  => U.Vector Particle -> (Particle -> Quantity d a) -> (Life -> Quantity d a)
accumByLife ps f life = accumBy f (lifeParticles life ps)

-- Fuel_AreaWtg
partAreaWtg :: Fuel -> Particle -> Ratio
partAreaWtg fuel part
  = partSurfaceArea part `safeDiv` accumBy partSurfaceArea sameLifeParticles
  where
    sameLifeParticles = lifeParticles (partLife part) (fuelParticles fuel)

-- Fuel_SizeAreaWtg
partSizeClassWtg :: Fuel -> Particle -> Ratio
partSizeClassWtg fuel part
 = accumBy (partAreaWtg fuel)
 . U.filter ((== partSizeClass part) . partSizeClass)
 $ lifeParticles (partLife part) (fuelParticles fuel)

data Combustion
  = Combustion {
      combLifeAreaWtg     :: Life -> Ratio -- ^ fuel area weighting factor
    , combLifeRxFactor    :: Life -> Ratio -- ^ fuel rx factor
    , combFineDeadFactor  :: Ratio -- ^ fine dead fuel ratio
    , combLiveExtFactor   :: Ratio -- ^ live fuel moisture extinction factor
    , combFuelBedBulkDens :: Density Double -- ^ fuel bed bulk density
    , combResidenceTime   :: Time Double -- ^ residence time
    , combFluxRatio       :: Ratio -- ^ propagating flux ratio
    , combSlopeK          :: Ratio -- ^ slope parameter 'k'
    , combWindB           :: Ratio -- ^ wind parameter 'b'
    , combWindE           :: Ratio -- ^ wind parameter 'e'
    , combWindK           :: Ratio -- ^ wind parameter 'k'
  }

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
    sigma :: Ratio
    sigma           = (lifeAreaWtg Alive * lifeSavr Alive
                    + lifeAreaWtg Dead  * lifeSavr Dead)
                    / (1 *~ perFoot)
    lifeRxFactor lf = lifeLoad lf * lifeHeat lf * lifeEtaS lf * gamma
    gamma           = gammaMax * (ratio ** aa) * exp (aa * (dl 1 - ratio))
      where
        gammaMax = sigma15 / (dl 495 + dl 0.0594 * sigma15)
        sigma15  = sigma ** dl 1.5
        aa       = dl 133 / (sigma ** dl 0.7913)
    ratio           = beta / (dl 3.348 / (sigma ** dl 0.8189))
    lifeEtaS lf
      | lifeSeff lf <= smidgen = 1
      | eta < 1                = eta
      | otherwise              = 1
      where eta = 0.174 / lifeSeff lf ** 0.19
    residenceTime   = (384 *~ second) / sigma
    fluxRatio       = exp ((0.792 + 0.681 * sqrt sigma) * (beta + 0.1))
                    / (192 + 0.2595*sigma)
    slopeK          = 5.275 * (beta ** (-0.3))
    windB           = dl 0.02526 * (sigma ** dl 0.54)
    (windK, windE)  = (c * (ratio ** (-e)), (ratio ** e) / c)
      where c = dl 7.47  * exp (dl (-0.133) * (sigma ** dl 0.55))
            e = dl 0.715 * exp (dl (-0.000359) * sigma)
    fineLive        = accumBy (\p -> partLoad p * exp ((-500) / partSavr p))
                    $ lifeParticles Alive particles
    fineDead        = accumBy (\p -> partLoad p / (1*~lbSqFt) * partSigmaFactor p)
                    $ lifeParticles Dead particles
    liveMextFactor  = 2.9 * fineDead `safeDiv` fineLive

    accumBy'
      :: (U.Unbox (Quantity d b), Num b)
      => (Particle -> Quantity d b) -> Quantity d b
    accumBy' f      = accumBy f particles

    accumByLife'
      :: (U.Unbox (Quantity d a), Num a)
      => (Particle -> Quantity d a) -> (Life -> Quantity d a)
    accumByLife'    = accumByLife particles

data Spread
  = Spread {
      spreadRxInt         :: Double -- ^ Reaction intensity (BTU/sqft/min)
    , spreadSpeed0        :: Double -- ^ no-wind, no-slope spread rate (ft/min)
    , spreadHpua          :: Double -- ^ heat per unit area (BTU/sqft)
    , spreadPhiEffWind    :: Double -- ^ combined wind-slope factor
    , spreadSpeedMax      :: Double -- ^ spread in direction of max spread
    , spreadAzimuthMax    :: Double -- ^ direction of max spread
    , spreadEccentricity  :: Double -- ^ eccentricity of the ellipse
    , spreadByramsMax     :: Double -- ^ fireline intensity in dir of max
                                    --   spread(BTU/ft/s)
    , spreadFlameMax      :: Double -- ^ flame length in dir of max spread (ft)
  } deriving (Eq, Show)

derivingUnbox "Spread"
    [t| Spread -> ( (Double,Double,Double,Double,Double)
                  , (Double,Double,Double,Double)) |]
    [| \(Spread a b c d e f g h i) -> ((a,b,c,d,e),(f,g,h,i)) |]
    [| \((a,b,c,d,e),(f,g,h,i)) -> Spread a b c d e f g h i|]

data SpreadAtAzimuth
  = SpreadAtAzimuth {
      spreadSpeed  :: Velocity Double -- ^ no-wind, no-slope spread rate (ft/min)
    , spreadByrams :: Double -- ^ fireline intensity (BTU/ft/s)
    , spreadFlame  :: Double -- ^ flame length (ft)
  } deriving (Eq, Show)

spreadAtAzimuth :: Spread -> Double -> SpreadAtAzimuth
spreadAtAzimuth Spread{..} azimuth
  = SpreadAtAzimuth {
      spreadSpeed  = spreadSpeedMax  * factor
    , spreadByrams = spreadByramsMax * factor
    , spreadFlame  = flameLength (spreadByramsMax * factor)
    }
  where
    factor
      | abs (azimuth - spreadAzimuthMax) < smidgen = dl 1
      | otherwise  = (dl 1 - spreadEccentricity) `safeDiv`
                     (dl 1 - spreadEccentricity * cos (degToRad angle))
    angle
      | ret > 180 = 360 - ret
      | otherwise = ret
      where ret = abs (spreadAzimuthMax - azimuth)



noSpread :: Spread
noSpread = Spread 0 0 0 0 0 0 0 0 0


data SpreadEnv
  = SpreadEnv {
      envD1hr        :: !Double
    , envD10hr       :: !Double
    , envD100hr      :: !Double
    , envD1000hr     :: !Double
    , envHerb        :: !Double
    , envWood        :: !Double
    , envWindSpeed   :: !Double
    , envWindAzimuth :: !Double
    , envSlope       :: !Double
    , envAspect      :: !Double
  } deriving (Eq, Show)

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

flameLength :: Double -> Double
flameLength byrams
  | byrams < smidgen = 0
  | otherwise        = 0.45 * (byrams ** 0.46)

data WindSlopeSituation
  = NoSpread
  | NoSlopeNoWind
  | WindNoSlope
  | SlopeNoWind
  | UpSlope
  | CrossSlope

degToRad :: Double -> Double
degToRad = (*) 0.017453293

radToDeg :: Double -> Double
radToDeg = (*) 57.29577951

infixr 8 ^!
(^!) :: Fractional a => a -> Int -> a
(^!) = (^)

derivingUnbox "SpreadEnv"
    [t| SpreadEnv -> ( (Double,Double,Double,Double,Double)
                     , (Double,Double,Double,Double,Double))|]
    [| \(SpreadEnv a b c d e f g h i j) -> ((a,b,c,d,e),(f,g,h,i,j)) |]
    [| \((a,b,c,d,e),(f,g,h,i,j)) -> SpreadEnv a b c d e f g h i j|]


newtype Catalog a = Catalog {unCatalog :: V.Vector a}
    deriving (Eq, Show)

catalogIndex :: Catalog a -> Int -> Maybe a
catalogIndex (Catalog v) = (V.!?) v

standardCatalog :: Catalog Fuel
standardCatalog = Catalog (V.fromList (map createFuel $ zip [0..] fuels))
  where
    fuels = [
            ("NoFuel", 0.1, 0.01, "No Combustible Fuel"),
            ("NFFL01", 1.0, 0.12, "Short Grass (1 ft)" ),
            ("NFFL02", 1.0, 0.15, "Timber (grass & understory)"),
            ("NFFL03", 2.5, 0.25, "Tall Grass (2.5 ft)"),
            ("NFFL04", 6.0, 0.20, "Chaparral (6 ft)"),
            ("NFFL05", 2.0, 0.20, "Brush (2 ft)"),
            ("NFFL06", 2.5, 0.25, "Dormant Brush & Hardwood Slash"),
            ("NFFL07", 2.5, 0.40, "Southern Rough"),
            ("NFFL08", 0.2, 0.30, "Closed Timber Litter"),
            ("NFFL09", 0.2, 0.25, "Hardwood Litter"),
            ("NFFL10", 1.0, 0.25, "Timber (litter & understory)"),
            ("NFFL11", 1.0, 0.15, "Light Logging Slash"),
            ("NFFL12", 2.3, 0.20, "Medium Logging Slash"),
            ("NFFL13", 3.0, 0.25, "Heavy Logging Slash")
            ]
    particles = [
            ( 1, ParticleDead, 0.0340, 3500),
            ( 2, ParticleDead, 0.0920, 3000),
            ( 2, ParticleDead, 0.0460, 109),
            ( 2, ParticleDead, 0.0230, 30),
            ( 2, ParticleHerb, 0.0230, 1500),
            ( 3, ParticleDead, 0.1380, 1500),
            ( 4, ParticleDead, 0.2300, 2000),
            ( 4, ParticleDead, 0.1840, 109),
            ( 4, ParticleDead, 0.0920, 30),
            ( 4, ParticleWood, 0.2300, 1500),
            ( 5, ParticleDead, 0.0460, 2000),
            ( 5, ParticleDead, 0.0230, 109),
            ( 5, ParticleWood, 0.0920, 1500),
            ( 6, ParticleDead, 0.0690, 1750),
            ( 6, ParticleDead, 0.1150, 109),
            ( 6, ParticleDead, 0.0920, 30),
            ( 7, ParticleDead, 0.0520, 1750),
            ( 7, ParticleDead, 0.0860, 109),
            ( 7, ParticleDead, 0.0690, 30),
            ( 7, ParticleWood, 0.0170, 1550),
            ( 8, ParticleDead, 0.0690, 2000),
            ( 8, ParticleDead, 0.0460, 109),
            ( 8, ParticleDead, 0.1150, 30),
            ( 9, ParticleDead, 0.1340, 2500),
            ( 9, ParticleDead, 0.0190, 109),
            ( 9, ParticleDead, 0.0070, 30),
            (10, ParticleDead, 0.1380, 2000),
            (10, ParticleDead, 0.0920, 109),
            (10, ParticleDead, 0.2300, 30),
            (10, ParticleWood, 0.0920, 1500),
            (11, ParticleDead, 0.0690, 1500),
            (11, ParticleDead, 0.2070, 109),
            (11, ParticleDead, 0.2530, 30),
            (12, ParticleDead, 0.1840, 1500),
            (12, ParticleDead, 0.6440, 109),
            (12, ParticleDead, 0.7590, 30),
            (13, ParticleDead, 0.3220, 1500),
            (13, ParticleDead, 1.0580, 109),
            (13, ParticleDead, 1.2880, 30)
            ]
    createFuel (i, (n,d,m,ds)) = Fuel n ds d m 1 (mkParts i)
    mkParts :: Int -> U.Vector Particle
    mkParts i                  = U.fromList . map mkPart . filterByIdx i
                               $ particles
    mkPart (_,t,l,s)           = Particle t l s 32 8000 0.0555 0.0100
    filterByIdx i              = filter (\(i',_,_,_) -> i == i')
