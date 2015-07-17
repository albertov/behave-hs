{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Behave.Types (
    Catalog
  , Particle (..)
  , ParticleType (..)
  , SizeClass (..)
  , Life (..)
  , Fuel (..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , Combustion (..)
  , noSpread
  , standardCatalog
  , indexCatalog
  , mkCatalog
) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Text (Text)
    

-- | Type of 'Particle'
data ParticleType
  = ParticleDead  -- ^ Dead fuel particle
  | ParticleHerb  -- ^ Herbaceous live particle
  | ParticleWood  -- ^ Woody live particle
  deriving (Show, Eq, Enum)

derivingUnbox "ParticleType"
    [t| ParticleType -> Int |]
    [|fromEnum|]
    [|toEnum|]


-- | A fuel bed particle
data Particle
  = Particle {
      partType        :: !ParticleType
    , partLoad        :: !Double         -- ^ fuel loading
    , partSavr        :: !Double         -- ^ surface area to volume ratio
    , partDensity     :: !Double         -- ^ particle density
    , partHeat        :: !Double         -- ^ heat of combustion
    , partSiTotal     :: !Double         -- ^ total silica content
    , partSiEffective :: !Double         -- ^ effective silica content
  } deriving (Eq, Show)

derivingUnbox "Particle"
    [t| Particle -> ((ParticleType,Double,Double),(Double,Double,Double,Double)) |]
    [| \(Particle a b c d e f g) -> ((a,b,c),(d,e,f,g)) |]
    [| \((a,b,c),(d,e,f,g)) -> Particle a b c d e f g|]

data Life = Dead | Alive deriving (Eq, Show, Enum)

data SizeClass = SC0 | SC1 | SC2 | SC3 | SC4 | SC5
  deriving (Eq, Show, Enum)


data Fuel
  = Fuel {
      fuelName      :: !Text                -- ^ name
    , fuelDesc      :: !Text                -- ^ description
    , fuelDepth     :: !Double              -- ^ total depth
    , fuelMext      :: !Double              -- ^ moisture of extinction
    , fuelAdjust    :: !Double              -- ^ adjustment factor
    , fuelParticles :: !(U.Vector Particle) -- ^ particle array
  } deriving (Eq, Show)

data Combustion
  = Combustion {
      combLifeAreaWtg     :: Life -> Double -- ^ fuel area weighting factor
    , combLifeRxFactor    :: Life -> Double -- ^ fuel rx factor
    , combFineDeadFactor  :: Double -- ^ fine dead fuel ratio
    , combLiveExtFactor   :: Double -- ^ live fuel moisture extinction factor
    , combFuelBedBulkDens :: Double -- ^ fuel bed bulk density
    , combResidenceTime   :: Double -- ^ residence time
    , combFluxRatio       :: Double -- ^ propagating flux ratio
    , combSlopeK          :: Double -- ^ slope parameter 'k'
    , combWindB           :: Double -- ^ wind parameter 'b'
    , combWindE           :: Double -- ^ wind parameter 'e'
    , combWindK           :: Double -- ^ wind parameter 'k'
  }

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

noSpread :: Spread
noSpread = Spread 0 0 0 0 0 0 0 0 0

derivingUnbox "Spread"
    [t| Spread -> ( (Double,Double,Double,Double,Double)
                  , (Double,Double,Double,Double)) |]
    [| \(Spread a b c d e f g h i) -> ((a,b,c,d,e),(f,g,h,i)) |]
    [| \((a,b,c,d,e),(f,g,h,i)) -> Spread a b c d e f g h i|]

data SpreadAtAzimuth
  = SpreadAtAzimuth {
      spreadSpeed  :: Double -- ^ no-wind, no-slope spread rate (ft/min)
    , spreadByrams :: Double -- ^ fireline intensity (BTU/ft/s)
    , spreadFlame  :: Double -- ^ flame length (ft)
  } deriving (Eq, Show)


data SpreadEnv
  = SpreadEnv {
      envD1hr        :: !Double -- ^ Moisture of 1hr lag dead particles
    , envD10hr       :: !Double -- ^ Moisture of 10hr lag dead particles
    , envD100hr      :: !Double -- ^ Moisture of 100hr lag dead particles
    , envHerb        :: !Double -- ^ Moisture of live herbaceous particles
    , envWood        :: !Double -- ^ Moisture of live woody particles
    , envWindSpeed   :: !Double -- ^ Wind speed
    , envWindAzimuth :: !Double -- ^ Wind azimuth (compass angle in degrees)
    , envSlope       :: !Double -- ^ Terrain slope (rise/reach ratio)
    , envAspect      :: !Double -- ^ Terrain aspect (downslope compass degress)
  } deriving (Eq, Show)

derivingUnbox "SpreadEnv"
    [t| SpreadEnv -> ( (Double,Double,Double,Double,Double)
                     , (Double,Double,Double,Double))|]
    [| \(SpreadEnv a b c d e f g h i) -> ((a,b,c,d,e),(f,g,h,i)) |]
    [| \((a,b,c,d,e),(f,g,h,i)) -> SpreadEnv a b c d e f g h i|]

-- | A Collection of fuels or combustion-intermediates
newtype Catalog a = Catalog (V.Vector a)
    deriving (Eq, Show, Functor)

indexCatalog :: Catalog a -> Int -> Maybe a
indexCatalog (Catalog v) = (V.!?) v
{-# INLINE indexCatalog #-}

mkCatalog :: [a] -> Catalog a
mkCatalog = Catalog . V.fromList

-- | The standard catalog
standardCatalog :: Catalog Fuel
standardCatalog = mkCatalog $ map createFuel $ zip [0..] fuels
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
