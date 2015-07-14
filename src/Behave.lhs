Implementación de BEHAVE (Andrews 1986) en Haskell
==================================================

El módulo BEHAVE comienza definiendo su nombre y los símbolos que exporta.
Cualquier símbolo no declarado aquí no se podrá utilizar fuera del módulo
permitiendo la encapsulación de detalles de la implementación.

> {-# LANGUAGE DataKinds #-}
> {-# OPTIONS_GHC -funbox-strict-fields #-}
> module Behave (
>     Particle (Particle)
>   , ParticleType (Dead, Herb, Wood)
>   , Fuel (Fuel)
>   , Catalog
>   , standardCatalog
>   , mkMoistures
> ) where

A continuación se importan los módulos externos de los que depende. Se utiliza
principalmente el paquete `Numeric.Units.Dimensional` que provee tipos numéricos
con dimensiones físicas asociadas. Éstos son comprobados por el sistema de tipos
de Haskell cuando el programa es compilado ayudando documentar y a prevenir
errores.

> import           Data.List (foldl')
> import           Numeric.Units.Dimensional.DK
> import           Numeric.Units.Dimensional.DK.Prelude hiding (Density)
> import           Numeric.Units.Dimensional.DK.NonSI (poundMass, foot)         
> import           Numeric.NumType.DK.Integers (TypeInt(..))

La librería `Prelude` se importa con nombre calíficado para evitar ambigüedad
con los operadores del mismo nombre y semántica que se importan de Dimensional
para operar con magnitudes físicas.

> import qualified Prelude as P

Definimos los tipos para las dimensiones físicas con las que trabajaremos.
La sintaxis para definirla especifica el exponente de cada dimensión del
Sistema internacional en un vector. Las dimensiones ordenadas son:

    1. Lóngitud
    2. Masa
    3. Tiempo
    4. Corriente eléctrica
    5. Temperatura termodinámica
    6. Cantidad de substancia
    7. Intensidad lumínica

Se ve mejor con un ejemplo:

> type DFuelLoad              = 'Dim 'Neg2 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero

Que se lee como:

 longitud ^-2, masa ^1, tiempo ^ 0, corriente ^ 0, ...

O lo que es lo mismo: la carga de combustible es una masa dividida entre un area
(longitud al cuadrado)


De la misma manera se definen tipos para dimensiones

> type DSaToVolRatio          = 'Dim 'Neg1 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
> type DHeatOfCombustion      = 'Dim 'Pos2 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
> type DHeatPerUnitArea       = 'Dim 'Zero 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
> type DReactionVelocity      = 'Dim 'Zero 'Zero 'Neg1 'Zero 'Zero 'Zero 'Zero

Y ahora los tipos para las cantidades (dimensión asociada a un valor)

> type FuelLoad                = Quantity DFuelLoad Double
> type SaToVolRatio            = Quantity DSaToVolRatio Double
> type Density                 = Quantity DDensity Double
> type HeatOfCombustion        = Quantity DHeatOfCombustion Double
> type HeatPerUnitArea         = Quantity DHeatPerUnitArea Double
> type ReactionVelocity        = Quantity DReactionVelocity Double
> type Ratio                   = Dimensionless Double
> type Fraction                = Dimensionless Double
> type Moisture                = Fraction
> type TotalMineralContent     = Fraction
> type EffectiveMineralContent = Fraction
> type Speed                   = Velocity Double
> type RateOfSpread            = Speed
> type Azimuth                 = PlaneAngle Double
> type Slope                   = Fraction
> type ReactionIntensity       = HeatFluxDensity Double


Ecuaciones de comportamiento del fuego de superficie
-----------------------------------------------------

Transcritas de "A Mathematical model for predicting fire spread in wildland
fuels" (Rothermel 1972) con los coeficientes adaptados al sistema internacional
"Reformulations of forest fire spread equations in SI units" (Wilson 1980)

Se les pone `'` de sufijo para diferenciarlas de las que operan sobre el
modelo de comnustible con todas sus partículas

Ecuación 52

> rateOfSpread' ::
>   Ratio             ->  -- Wind coefficient
>   Ratio             ->  -- Slope Ratio
>   ReactionIntensity ->  -- Reaction intensity
>   Ratio             ->  -- propagating flux ratio
>   Density           ->  -- bulk density
>   Ratio             ->  -- effective heating number
>   HeatOfCombustion  ->  -- heat of preignition
>   RateOfSpread
> rateOfSpread' phiW phiS ir e pb e' qig
>     = ir * e * (_1 + phiW + phiS)
>     / (pb * e' * qig)

> rateOfSpread0' ::
>   ReactionIntensity ->  -- Reaction intensity
>   Ratio             ->  -- propagating flux ratio
>   Density           ->  -- bulk density
>   Ratio             ->  -- effective heating number
>   HeatOfCombustion  ->  -- heat of preignition
>   RateOfSpread
> rateOfSpread0' = rateOfSpread' _0 _0

Ecuación 27

> reactionIntensity' ::
>   ReactionVelocity ->  -- optimum reaction velocity
>   FuelLoad         ->  -- net fuel loading
>   HeatOfCombustion ->  -- fuel particle heat content
>   Ratio            ->  -- moisture damping coefficient
>   Ratio            ->  -- mineral damping coefficient
>   ReactionIntensity
> reactionIntensity' r w h m s = r * w * h * m * s

Ecuaciones 38 y 36

> optimumReactionVelocity' :: SaToVolRatio -> Fraction -> ReactionVelocity
> optimumReactionVelocity' s b = orv `asUnits` (minute ^ neg1)
>  where
>     orv  = rmax * ((b / bop) ** a) * exp (a * ((_1 - b) / bop))
>     a    = (((6.7229 *~ hecto meter) * s) ** (0.1 *~ one)) - (7.27 *~ one)
>     bop  = optimumPackingRatio' s
>     rmax = dl 0.0591 + (((2.926 *~ hecto meter) * s) ** dl (-1.5))

Ecuación 37

> optimumPackingRatio' :: SaToVolRatio -> Fraction
> optimumPackingRatio' s = ((0.20395 *~ hecto meter) * s) ** dl (-0.8189)


Ecuación 29

> moistureDampingRatio' ::
>   Moisture ->  -- Fuel moisture
>   Moisture ->  -- Extinction moisture
>   Ratio      
> moistureDampingRatio' m mx
>    = _1
>    - dl 2.59 *  m / mx
>    + dl 5.11 * ((m / mx) ** dl 2)
>    - dl 3.52 * ((m / mx) ** dl 3)

Ecuación 30

> mineralDampingCoeffient' ::
>   EffectiveMineralContent -> Fraction
> mineralDampingCoeffient' se = dl 0.174 * (se ** dl (-0.19))

Ecuación 42

> propagatingFluxRatio' ::
>   SaToVolRatio ->  -- particle surafce area to volume ratio
>   Ratio        ->  -- packing ratio
>   Ratio      
> propagatingFluxRatio' s b
>     = exp ( (dl 0.792 + sqrt((3.7597 *~ hecto meter) * s)) * (b + dl 0.1) )
>     / (dl 192 + ((7.9095 *~ hecto meter) * s))

Ecuaciones 47, 48, 49 y 50 (TODO)

> windRatio' = undefined

Ecuación 24

> netFuelLoading' :: FuelLoad -> Fraction -> FuelLoad
> netFuelLoading'  wo st = wo / (_1 - st)


Ecuación 51

> slopeRatio' = undefined


Ecuación 40

> bulkDensity' :: FuelLoad -> (Length Double) -> Density
> bulkDensity' load depth = load / depth


Ecuación 14

> effectiveHeatingNumber' :: SaToVolRatio -> Ratio
> effectiveHeatingNumber' s = exp ( ((-4.528) *~ ((hecto meter) ^ neg1)) / s )

Ecuación 12

> heatOfPreignition' :: Moisture -> HeatOfCombustion
> heatOfPreignition' m = (dl 581 + (dl 2594 * m)) `asUnits` (joule/gram)

Ecuación 31

> packingRatio' :: Density -> Density -> Ratio
> packingRatio' pb pp = pb / pp


Formulación del modelo de combustible
-------------------------------------

El combustible tiene asociado un nombre, descripción, profundidad, humedad de
exitinción, factor de ajuste y un conjunto de particulas.

> data Fuel
>   = Fuel {
>       name      :: !String             -- Fuel model name
>     , desc      :: !String             -- Fuel model description
>     , depth     :: !(Length Double)    -- Fuel model total depth
>     , mext      :: !Moisture           -- Fuel model moisture of extinction
>     , adjust    :: !Ratio              -- Fuel model adjustment factor
>     , particles :: [Particle]          -- Fuel model particle array
>   } deriving (Eq, Show)


Necesitaremos un catalogo para buscar los combustibles por identificador
numérico. Una lista de combustibles servirá.

> type Catalog = [Fuel]

Los combustibles se pueden extraer del catálogo por identificador numérico.
Para ello utilizamos el operador de indexado pero le ponemos un nombre para
poder cambiar la implementación sin modificar los clientes.

> fuelById :: Catalog -> Int -> Fuel
> fuelById = (!!)


Las partículas del combustible tienen un tipo de partícula asociado y parámetros
intrínsecos.

> data Particle
>   = Particle {
>       type_:: !ParticleType
>     , load :: !FuelLoad
>     , savr :: !SaToVolRatio
>     , dens :: !Density
>     , heat :: !HeatOfCombustion
>     , mtot :: !TotalMineralContent
>     , meff :: !EffectiveMineralContent
>   } deriving (Eq, Show)

El tipo de partícula. Puede ser "muerto", "herbaceo" o "leñoso".

> data ParticleType = Dead | Herb | Wood deriving (Show, Eq)


Para ponderar la contribución de cada partícula al fuego se las clasifica
por tipo de partícula y tamaño.

> data LifeClass = LCDead | LCAlive deriving (Eq,Show,Enum)
>
> lifeClasses :: [LifeClass]
> lifeClasses = enumFrom LCDead
>
> data SizeClass = SC0 | SC1 | SC2 | SC3 | SC4 | SC5 deriving (Eq,Show,Enum)
>
> sizeClasses :: [SizeClass]
> sizeClasses = enumFrom SC0

La clasificamos por tamaño en función de su ratio de area/volumen

> sizeClass :: Particle -> SizeClass
> sizeClass p =
>     fst . head . P.dropWhile (\(_,v) -> savr p < v) $ sizeClassVolRatio
>   where
>     sizeClassVolRatio :: [(SizeClass, SaToVolRatio)] 
>     sizeClassVolRatio = zip sizeClasses [
>           1200 *~ perFoot
>         , 192  *~ perFoot
>         , 96   *~ perFoot
>         , 48   *~ perFoot
>         , 16   *~ perFoot
>         , 0    *~ perFoot
>         ]

Y por si está viva o muerta

> lifeClass :: Particle -> LifeClass
> lifeClass p = case type_ p of
>                    Dead -> LCDead
>                    _    -> LCAlive

Cada tupla (clase de vida, clase de tamaño) se denomina
"célula de combustible unitaria" (unit fuel cell) y se usará para clasificar
las particulas de un modelo de combustible y calcular los agregados de cada
parámetro para calcular su contrubución ponderada al fuego.

> type FuelCell = (LifeClass, SizeClass)

Se define una función  para calcular la media de cualquier parámetro de una
célula de combustible, se usará para abstraer el computo de dicha media en
las sucesivas definiciones

> meanCellBy fn fuel cell = sum' / ((fromIntegral len') *~ one)
>   where (sum',len') = foldl' fl (_0, 0) particles'
>         fl (s,l) p  = (s + fn p, l P.+ 1)
>         particles'  = filter filterFn $ particles fuel
>         filterFn p  = lifeClass p == i && sizeClass p == j
>         (i, j)      = cell

La superficie expuesta al fuego media de cada célula se define el producto de
del ratio superficie/volumen medio por la carga media entre la densidad media de
las particulas que se clasifican en dicha célula

> meanCellSurfaceArea :: Fuel -> FuelCell -> Dimensionless Double
> meanCellSurfaceArea fuel cell = (mean savr) * (mean load) / (mean dens)
>    where mean fn = meanCellBy fn fuel cell

Todas las clases de tamaño que corresponden a una misma clase de vida se
denomina "categoría" y su superficie media es la suma de las medias de las
células que la componen:

> meanCategorySurfaceArea :: Fuel -> LifeClass -> Dimensionless Double
> meanCategorySurfaceArea fuel i =
>     sum . map (\j -> meanCellSurfaceArea fuel (i,j)) $ sizeClasses

La superficie media de un modelo de combustible es la suma de la media de sus
categorías:

> meanSurfaceArea :: Fuel -> Dimensionless Double
> meanSurfaceArea fuel = sum . map (meanCategorySurfaceArea fuel) $ lifeClasses

Se definen los factores de ponderación de cada célula y categoría de
combustible en función de las superfices medias:

> cellFactor :: Fuel -> FuelCell -> Ratio
> cellFactor fuel (i, j) = meanCellSurfaceArea fuel (i, j)
>                        / meanCategorySurfaceArea fuel i
>
> categoryFactor :: Fuel -> LifeClass -> Ratio
> categoryFactor fuel i = meanCategorySurfaceArea fuel i
>                       / meanSurfaceArea fuel




Definimos un tipo para guardar las humedades de cada clase de partícula.
un constructor a partir de una lista de reales y una función para extraer
la humedad asocidad a una partícula.

> data Moistures = Moistures {
>       d1hr    :: !Moisture
>     , d10hr   :: !Moisture
>     , d100hr  :: !Moisture
>     , d1000hr :: !Moisture
>     , herb    :: !Moisture
>     , wood    :: !Moisture
> } deriving (Show)

> mkMoistures
>   :: Double -> Double -> Double -> Double -> Double -> Double
>   -> Maybe Moistures
> mkMoistures a b c d e f
>   = Moistures <$> toFraction a
>               <*> toFraction b
>               <*> toFraction c
>               <*> toFraction d
>               <*> toFraction e
>               <*> toFraction f

> particleMoisture :: Particle -> Moistures -> Moisture
> particleMoisture p = let ix = fromEnum . sizeClass $ p in
>    case type_ p of
>      Herb -> herb
>      Wood -> wood
>      Dead -> [d1hr, d1hr, d10hr, d10hr, d100hr, d100hr] !! ix



Una función para crear fracciones asegurando que su valor está entre 0 y 1

> toFraction :: (Num a, Ord a) => a -> Maybe (Quantity DOne a)
> toFraction v
>     | v>=0 && v<=1 = Just (v *~ one)
>     | otherwise    = Nothing

> data Wind    = Wind    !Speed !Azimuth 
> data Terrain = Terrain !Slope !Azimuth 


Utilidades para operar con valores adimensionales

> dl :: Num a => a -> Dimensionless a
> dl v = v *~ one

> asUnits :: Fractional a => Dimensionless a -> Unit d a -> Quantity d a
> asUnits v u = (v /~ one) *~ u

A continuación se definen tipos para las unidades del sistema imperial en las
que introduciremos los parámetros del modelo estándar.
La librería Dimensions se encarga de la conversión desde y hacia el sistema
métrico internacional.

> lbSqFt :: Unit DFuelLoad Double
> lbSqFt = poundMass/(foot ^ pos2)

> lbCuFt :: Unit DDensity Double
> lbCuFt = poundMass/(foot ^ pos3)

> btu:: Unit DEnergy Double
> btu = prefix 0.293071 (watt * hour)

> btuLb:: Unit DHeatOfCombustion Double
> btuLb = btu / poundMass

> perFoot :: Unit DSaToVolRatio Double
> perFoot  = foot ^ neg1

El catálogo estándar:

> standardCatalog :: Catalog
> standardCatalog =
>     let fuels = [
>             ("NoFuel", 0.1, 0.01, "No Combustible Fuel"),
>             ("NFFL01", 1.0, 0.12, "Short Grass (1 ft)" ),
>             ("NFFL02", 1.0, 0.15, "Timber (grass & understory)"),
>             ("NFFL03", 2.5, 0.25, "Tall Grass (2.5 ft)"),
>             ("NFFL04", 6.0, 0.20, "Chaparral (6 ft)"),
>             ("NFFL05", 2.0, 0.20, "Brush (2 ft)"),
>             ("NFFL06", 2.5, 0.25, "Dormant Brush & Hardwood Slash"),
>             ("NFFL07", 2.5, 0.40, "Southern Rough"),
>             ("NFFL08", 0.2, 0.30, "Closed Timber Litter"),
>             ("NFFL09", 0.2, 0.25, "Hardwood Litter"),
>             ("NFFL10", 1.0, 0.25, "Timber (litter & understory)"),
>             ("NFFL11", 1.0, 0.15, "Light Logging Slash"),
>             ("NFFL12", 2.3, 0.20, "Medium Logging Slash"),
>             ("NFFL13", 3.0, 0.25, "Heavy Logging Slash")
>             ]
>         particles = [
>             ( 1, Dead, 0.0340, 3500),
>             ( 2, Dead, 0.0920, 3000),
>             ( 2, Dead, 0.0460, 109),
>             ( 2, Dead, 0.0230, 30),
>             ( 2, Herb, 0.0230, 1500),
>             ( 3, Dead, 0.1380, 1500),
>             ( 4, Dead, 0.2300, 2000),
>             ( 4, Dead, 0.1840, 109),
>             ( 4, Dead, 0.0920, 30),
>             ( 4, Wood, 0.2300, 1500),
>             ( 5, Dead, 0.0460, 2000),
>             ( 5, Dead, 0.0230, 109),
>             ( 5, Wood, 0.0920, 1500),
>             ( 6, Dead, 0.0690, 1750),
>             ( 6, Dead, 0.1150, 109),
>             ( 6, Dead, 0.0920, 30),
>             ( 7, Dead, 0.0520, 1750),
>             ( 7, Dead, 0.0860, 109),
>             ( 7, Dead, 0.0690, 30),
>             ( 7, Wood, 0.0170, 1550),
>             ( 8, Dead, 0.0690, 2000),
>             ( 8, Dead, 0.0460, 109),
>             ( 8, Dead, 0.1150, 30),
>             ( 9, Dead, 0.1340, 2500),
>             ( 9, Dead, 0.0190, 109),
>             ( 9, Dead, 0.0070, 30),
>             (10, Dead, 0.1380, 2000),
>             (10, Dead, 0.0920, 109),
>             (10, Dead, 0.2300, 30),
>             (10, Wood, 0.0920, 1500),
>             (11, Dead, 0.0690, 1500),
>             (11, Dead, 0.2070, 109),
>             (11, Dead, 0.2530, 30),
>             (12, Dead, 0.1840, 1500),
>             (12, Dead, 0.6440, 109),
>             (12, Dead, 0.7590, 30),
>             (13, Dead, 0.3220, 1500),
>             (13, Dead, 1.0580, 109),
>             (13, Dead, 1.2880, 30)
>             ]
>         createFuel (i, (n,d,m,ds)) = Fuel n ds (d*~foot) (m*~one) _1 (mkParts i)
>         mkParts i                  = map mkPart . filterByIdx i $ particles
>         mkPart (_,t,l,s)           = Particle
>                                            t
>                                            (l      *~ lbSqFt)
>                                            (s      *~ perFoot)
>                                            (32.0   *~ lbCuFt)
>                                            (8000   *~ btuLb)
>                                            (0.0555 *~ one)
>                                            (0.0100 *~ one)
>         filterByIdx i              = filter (\(i',_,_,_) -> i == i')
>     in map createFuel $ zip [0..] fuels
