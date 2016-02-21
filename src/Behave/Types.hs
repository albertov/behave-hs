{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Behave.Types (
    Particle (..)
  , ParticleType (..)
  , SizeClass (..)
  , Life (..)
  , Fuel (..)
  , SpreadEnv (..)
  , Spread (..)
  , SpreadAtAzimuth (..)
  , Combustion (..)
  , PreparedFuel
  , noSpread
  , noSpreadEnv
  , standardCatalog
  , indexCatalog
  , _envWindSpeed
) where

import           Control.Lens (makeLensesFor)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Text (Text)
import           Behave.Units
import           Data.Hashable (Hashable(..))


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
    , partLoad        :: !FuelLoad         -- ^ fuel loading
    , partSavr        :: !SaToVolRatio     -- ^ surface area to volume ratio
    , partDensity     :: !(Density Double) -- ^ particle density
    , partHeat        :: !HeatOfCombustion -- ^ heat of combustion
    , partSiTotal     :: !Fraction         -- ^ total silica content
    , partSiEffective :: !Fraction         -- ^ effective silica content
  } deriving (Eq, Show)

derivingUnbox "Particle"
    [t| Particle -> ( (ParticleType,FuelLoad,SaToVolRatio)
                    , (Density Double,HeatOfCombustion,Fraction,Fraction)) |]
    [| \(Particle a b c d e f g) -> ((a,b,c),(d,e,f,g)) |]
    [| \((a,b,c),(d,e,f,g)) -> Particle a b c d e f g|]

data Life = Dead | Alive deriving (Eq, Show, Enum)

data SizeClass = SC0 | SC1 | SC2 | SC3 | SC4 | SC5
  deriving (Eq, Show, Enum)


data Fuel
  = Fuel {
      fuelName      :: !Text                   -- ^ name
    , fuelDesc      :: !Text                   -- ^ description
    , fuelDepth     :: !(Length Double)        -- ^ total depth
    , fuelMext      :: !Moisture               -- ^ moisture of extinction
    , fuelAdjust    :: !(Dimensionless Double) -- ^ adjustment factor
    , fuelParticles :: !(U.Vector Particle)    -- ^ particle array
  } deriving (Eq, Show)

data Combustion
  = Combustion {
      combLiveAreaWtg     :: !Double -- ^ fuel area weighting factor
    , combLiveRxFactor    :: !Double -- ^ fuel rx factor
    , combDeadAreaWtg     :: !Double -- ^ fuel area weighting factor
    , combDeadRxFactor    :: !Double -- ^ fuel rx factor
    , combFineDeadFactor  :: !Double -- ^ fine dead fuel ratio
    , combLiveExtFactor   :: !Double -- ^ live fuel moisture extinction factor
    , combFuelBedBulkDens :: !Double -- ^ fuel bed bulk density
    , combResidenceTime   :: !(Time Double)  -- ^ residence time
    , combFluxRatio       :: !Double -- ^ propagating flux ratio
    , combSlopeK          :: !Double -- ^ slope parameter 'k'
    , combWindB           :: !Double -- ^ wind parameter 'b'
    , combWindE           :: !Double -- ^ wind parameter 'e'
    , combWindK           :: !Double -- ^ wind parameter 'k'
  }

derivingUnbox "Combustion"
    [t| Combustion -> ( (Double,Double,Double,Double,Double)
                      , (Double,Double,Time Double,Double, Double)
                      , (Double,Double,Double)) |]
    [| \(Combustion a b c d e f g h i j k l m)
      -> ((a,b,c,d,e),(f,g,h,i,j),(k,l,m)) |]
    [| \((a,b,c,d,e),(f,g,h,i,j),(k,l,m))
      -> Combustion a b c d e f g h i j k l m|]

type PreparedFuel = (Fuel, Combustion)

data Spread
  = Spread {
      spreadRxInt         :: !ReactionIntensity -- ^ Reaction intensity
    , spreadSpeed0        :: !Speed             -- ^ no-wind, no-slope speed
    , spreadHpua          :: !HeatPerUnitArea   -- ^ heat per unit area
    , spreadPhiEffWind    :: !Ratio             -- ^ combined wind-slope factor
    , spreadSpeedMax      :: !Speed             -- ^ max spread speed
    , spreadAzimuthMax    :: !Azimuth           -- ^ direction of max spread
    , spreadEccentricity  :: !Ratio             -- ^ ellipse eccentricity
    , spreadByramsMax     :: !ByramsIntensity   -- ^ max fireline intensity
    , spreadFlameMax      :: !(Length Double)   -- ^ max flame length
  } deriving (Eq, Show)

instance Hashable Spread where
  hashWithSalt s (Spread a b c d e f g h i) =
    s `hashWithSalt` a
      `hashWithSalt` b
      `hashWithSalt` c
      `hashWithSalt` d
      `hashWithSalt` e
      `hashWithSalt` f
      `hashWithSalt` g
      `hashWithSalt` h
      `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

noSpread :: Spread
noSpread = Spread _0 _0 _0 _0 _0 _0 _0 _0 _0

derivingUnbox "Spread"
    [t| Spread -> ( (ReactionIntensity,Speed,HeatPerUnitArea,Ratio,Speed)
                  , (Azimuth,Ratio,ByramsIntensity,Length Double)) |]
    [| \(Spread a b c d e f g h i) -> ((a,b,c,d,e),(f,g,h,i)) |]
    [| \((a,b,c,d,e),(f,g,h,i)) -> Spread a b c d e f g h i|]

data SpreadAtAzimuth
  = SpreadAtAzimuth {
      spreadSpeed  :: Speed           -- ^ no-wind, no-slope spread rate
    , spreadByrams :: ByramsIntensity -- ^ fireline intensity
    , spreadFlame  :: Length Double   -- ^ flame Length
  } deriving (Eq, Show)

derivingUnbox "SpreadAtAzimuth"
    [t| SpreadAtAzimuth -> (Speed,ByramsIntensity,Length Double) |]
    [| \(SpreadAtAzimuth a b c) -> (a,b,c) |]
    [| \(a,b,c) -> SpreadAtAzimuth a b c|]

data SpreadEnv
  = SpreadEnv {
      envD1hr        :: !Moisture -- ^ Moisture of 1hr lag dead particles
    , envD10hr       :: !Moisture -- ^ Moisture of 10hr lag dead particles
    , envD100hr      :: !Moisture -- ^ Moisture of 100hr lag dead particles
    , envHerb        :: !Moisture -- ^ Moisture of live herbaceous particles
    , envWood        :: !Moisture -- ^ Moisture of live woody particles
    , envWindSpeed   :: !Speed    -- ^ Wind speed
    , envWindAzimuth :: !Azimuth  -- ^ Wind azimuth (compass bearing)
    , envSlope       :: !Ratio    -- ^ Terrain slope (rise/reach ratio)
    , envAspect      :: !Azimuth  -- ^ Terrain aspect (downslope compass bearing)
  } deriving (Eq, Show)
makeLensesFor [
   ("envWindSpeed", "_envWindSpeed")
  ] ''SpreadEnv

noSpreadEnv :: SpreadEnv
noSpreadEnv = SpreadEnv _0 _0 _0 _0 _0 _0 _0 _0 _0

instance Hashable SpreadEnv where
  hashWithSalt s (SpreadEnv a b c d e f g h i) =
    s `hashWithSalt` a
      `hashWithSalt` b
      `hashWithSalt` c
      `hashWithSalt` d
      `hashWithSalt` e
      `hashWithSalt` f
      `hashWithSalt` g
      `hashWithSalt` h
      `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

derivingUnbox "SpreadEnv"
    [t| SpreadEnv -> ( (Moisture,Moisture,Moisture,Moisture,Moisture)
                     , (Speed,Azimuth,Ratio,Azimuth))|]
    [| \(SpreadEnv a b c d e f g h i) -> ((a,b,c,d,e),(f,g,h,i)) |]
    [| \((a,b,c,d,e),(f,g,h,i)) -> SpreadEnv a b c d e f g h i|]

indexCatalog :: G.Vector v a => v a -> Int -> Maybe a
indexCatalog = (G.!?)
{-# INLINE indexCatalog #-}

-- | The standard catalog
standardCatalog :: V.Vector Fuel
standardCatalog = V.imap createFuel fuels
  where
    fuels = V.fromList [
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
    createFuel i (n,d,m,ds)    = Fuel n ds (d*~foot) (m*~one) _1 (mkParts i)
    mkParts :: Int -> U.Vector Particle
    mkParts i                  = U.fromList . map mkPart . filterByIdx i
                               $ particles
    mkPart (_,t,l,s)           = Particle
                                       t
                                       (l      *~ lbSqFt)
                                       (s      *~ perFoot)
                                       (32.0   *~ lbCuFt)
                                       (8000   *~ btuLb)
                                       (0.0555 *~ one)
                                       (0.0100 *~ one)
    filterByIdx i              = filter (\(i',_,_,_) -> i == i')
