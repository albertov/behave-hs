Implementación de BEHAVE (Andrews 1986) en Haskell
==================================================

Por Alberto Valevrde González

El módulo BEHAVE comienza definiendo su nombre y los símbolos que exporta.
Cualquier símbolo no declarado aquí no se podrá utilizar fuera del módulo
permitiendo la encapsulación de detalles de la implementación.

> module Behave (
>     Particle (Particle),
>     mkParticle,
>     ParticleType (Dead, Herb, Wood),
>     Fuel (Fuel),
>     Catalog,
>     standardCatalog
> ) where

A continuación se importan los módulos externos de los que depende. Se utiliza
principalmente el paquete `Numeric.Units.Dimensional` que provee tipos numéricos
con dimensiones físicas asociadas. Éstos son comprobados por el sistema de tipos
de Haskell cuando el programa es compilado ayudando documentar y a prevenir
errores.

> import           Numeric.Units.Dimensional (Dimensional)
> import           Numeric.Units.Dimensional.Prelude hiding (Density)
> import           Numeric.Units.Dimensional.NonSI (poundMass, foot)         
> import           Numeric.NumType (Neg1, Neg2, Neg3, Pos1, Pos2, Zero)

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

> type DFuelLoad              = Dim Neg2 Pos1 Zero Zero Zero Zero Zero

Que se lee como:

 longitud ^-2, masa ^1, tiempo ^ 0, corriente ^ 0, ...

O lo que es lo mismo: la carga de combustible es una masa dividida entre un area
(longitud al cuadrado)


De la misma manera se definen tipos para dimensiones

> type DSurfaceAreaToVolume   = Dim Neg1 Zero Zero Zero Zero Zero Zero
> type DDensity               = Dim Neg3 Pos1 Zero Zero Zero Zero Zero
> type DHeatOfCombustion      = Dim Pos2 Zero Neg2 Zero Zero Zero Zero
> type DHeatPerUnitArea       = Dim Zero Pos1 Neg2 Zero Zero Zero Zero

Y ahora los tipos para las cantidades (dimensión asociada a un valor)

> type FuelLoad               = Quantity DFuelLoad Double
> type SurfaceAreaToVolume    = Quantity DSurfaceAreaToVolume Double
> type Density                = Quantity DDensity Double
> type HeatOfCombustion       = Quantity DHeatOfCombustion Double
> type HeatPerUnitArea        = Quantity DHeatPerUnitArea Double
> type Fraction               = Dimensionless Double
> type Moisture               = Fraction
> type TotalSilicaContent     = Fraction
> type EffectiveSilicaContent = Fraction


A continuación se definen tipos para las unidades del sistema imperial en las que
el programa original trabaja. La librería Dimensions se encarga de la conversión
desde y hacia el sistema métrico internacional.

> lbSqFt :: Unit DFuelLoad Double
> lbSqFt = poundMass/(foot ^ pos2)

> lbCuFt :: Unit DDensity Double
> lbCuFt = poundMass/(foot ^ pos3)

> wattHour, btu:: Unit DEnergy Double
> wattHour = watt * hour
> btu = prefix 0.293071 wattHour

> btuLb:: Unit DHeatOfCombustion Double
> btuLb = btu / poundMass

> perFoot :: Unit DSurfaceAreaToVolume Double
> perFoot  = foot ^ neg1


Definimos el tipo de partícula. Éste puede tomar los valores de "muerto",
"herbaceo" o "leñoso".

> data ParticleType = Dead | Herb | Wood deriving (Show, Eq)

Las partículas del combustible tienen un tipo de partícula asociado y parámetros
intrínsecos.

> data Particle = Particle {
>     type_:: ParticleType,
>     load :: FuelLoad,
>     savr :: SurfaceAreaToVolume,
>     dens :: Density,
>     heat :: HeatOfCombustion,
>     stot :: TotalSilicaContent,
>     seff :: EffectiveSilicaContent
> } deriving (Eq, Show)

Una partícula está viva si es de tipo herbaceo o leñoso

> isAlive :: Particle -> Bool
> isAlive (Particle Dead _ _ _ _ _ _ ) = False
> isAlive (Particle Herb _ _ _ _ _ _ ) = True
> isAlive (Particle Wood _ _ _ _ _ _ ) = True


Hacemos una función pata construir una partícula de manera cómoda estableciendo
algunos parámetros por defecto.

> mkParticle :: ParticleType -> FuelLoad -> SurfaceAreaToVolume -> Particle
> mkParticle t l s =
>     Particle 
>         t
>         l
>         s
>         (32.0   *~ lbCuFt)
>         (8000   *~ btuLb)
>         (0.0555 *~ one)
>         (0.0100 *~ one)

El combustible tiene asociado un nombre, descripción, profundidad, humedad de
exitinción, factor de ajuste y una lista de particulas.

> data Fuel = Fuel {
>     name      :: String,
>     desc      :: String,
>     depth     :: Length Double,
>     mext      :: Moisture,
>     adjust    :: Dimensionless Double,
>     particles :: [Particle]
> } deriving (Eq, Show)

Un catálogo es simplemente una lista de combustibles.

> type Catalog = [Fuel]


Los combustibles se pueden extraer del catálogo por identificador numérico.
Éste es su posición en el catálogo comenzando por 0.

> getFuel :: Catalog -> Int -> Fuel
> getFuel catalog idx = catalog!!idx


Definimos el catálogo estándar:


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
>         mkPart (_,t,l,s)           = mkParticle t (l*~lbSqFt) (s*~perFoot)
>         filterByIdx i              = filter (\(i',_,_,_) -> i == i')
>     in map createFuel $ zip [0..] fuels

A continuación se definen las ecuaciones para calcular los parámetros de las
partículas y modelos de combustible que se pueden derivar de los intrínsecos.

Comenzamos definiendo un épsilon muy pequeño para ver si los valores de cierta
unidad son distintos de cero. Ésto es necesario debido a como se representan
en la máquina los números en coma flotante.

> smidgen u = 1e-6 *~ u
> nonZero v u = abs v > smidgen u

Los nombres intentan reflejar los originales aunque se puede ver, por el tipo
de dimensiones que devuelven, que estos no eran muy precisos. Por ejemplo,
`surfaceArea` es adimensional cuando su nombre sugiere que devuelve un area.

> surfaceArea :: Particle -> Dimensionless Double
> surfaceArea p = load p * savr p / dens p

> sigmaFactor :: Particle -> Dimensionless Double
> sigmaFactor p
>     | nonZero (savr p) perFoot = exp ((-138) *~ perFoot / savr p)
>     | otherwise                = _0

> sizeClass :: Particle -> Int
> sizeClass p =
>     toEnum . fst . head . P.dropWhile gtThanSavr $ zip [0..] size_boundary
>     where size_boundary    = [1200, 192, 96, 48, 16, 0]
>           gtThanSavr (_,v) = savr p < v *~ perFoot

Vamos a necesitar agregar ciertos computos que operan sobre todas las
partículas de un modelo de combustible por lo que definimos algunas funciones
que nos ayuden.

`overFilteredParticles` aplica una funcion sobre todas las particulas que pasen
un filtro y suma los resultados.

> overFilteredParticles
>   :: Num a =>
>      (Particle -> Quantity d a) -> 
>      (Particle -> Bool) ->
>       Fuel ->
>       Quantity d a
> overFilteredParticles mfunc ffunc = sum . map mfunc . filter ffunc . particles

`overParticles` aplica una funcion sobre todas las particulas y suma los
resultados, sin filtrar.

> overParticles :: Num a => (Particle -> Quantity d a) -> Fuel -> Quantity d a
> overParticles f = sum . map f . particles


> totalAreaBy :: (Particle -> Bool) -> Fuel -> Dimensionless Double
> totalAreaBy = overFilteredParticles surfaceArea

> totalArea :: Fuel -> Dimensionless Double
> totalArea = overParticles surfaceArea


> areaWeight :: Fuel -> Particle -> Dimensionless Double
> areaWeight f p = surfaceArea p / totalAreaOfSameLiveness
>     where totalAreaOfSameLiveness = totalAreaBy (\p'->isAlive p == isAlive p') f

> overParticlesPerDepth f fuel = (overParticles f fuel) / depth fuel

> bulkDensity :: Fuel -> Density
> bulkDensity = overParticlesPerDepth load

> beta :: Fuel -> Dimensionless Double
> beta = overParticlesPerDepth (\p -> load p / dens p)

> sigma :: Fuel -> Dimensionless Double
> sigma fuel = sigma' isAlive + sigma' (not.isAlive)
>     where sigma' f = (totalAreaWtBy f fuel) * (totalSavrBy f fuel) * (1 *~foot)

> totalAreaWtBy :: (Particle -> Bool) -> Fuel -> Dimensionless Double
> totalAreaWtBy func fuel = (totalAreaBy func fuel) / totalArea fuel


> residenceTime :: Fuel -> Time Double
> residenceTime fuel = (384 *~ minute) / sigma fuel

> sizeAreaWeight :: Fuel -> Particle -> Dimensionless Double
> sizeAreaWeight f p = overFilteredParticles (areaWeight f) filter' $ f
>     where filter' p' = sizeClass p == sizeClass p' && isAlive p == isAlive p'

> totalLoadBy :: (Particle -> Bool) -> Fuel -> FuelLoad
> totalLoadBy f fuel =
>     overFilteredParticles
>     (\p -> (sizeAreaWeight fuel p) * load p * (_1 - (stot p)))
>     f fuel

> overFilteredParticlesTimesAw mfunc ffunc fuel =
>     overFilteredParticles (\p -> (areaWeight fuel p) * mfunc p) ffunc fuel

> totalSavrBy :: (Particle -> Bool) -> Fuel -> SurfaceAreaToVolume
> totalSavrBy = overFilteredParticlesTimesAw savr

> totalHeatBy :: (Particle -> Bool) -> Fuel -> HeatOfCombustion
> totalHeatBy = overFilteredParticlesTimesAw heat

> totalSeffBy :: (Particle -> Bool) -> Fuel -> Fraction
> totalSeffBy = overFilteredParticlesTimesAw seff

> totalEtaSBy :: (Particle -> Bool) -> Fuel -> Fraction
> totalEtaSBy f fuel
>     | eta' <= _1 && (totalSeffBy f fuel) > _0 = eta'
>     | otherwise                               = _1
>     where eta'  = (0.174 *~ one) / ((totalSeffBy f fuel) ** (0.19 *~ one))

> rxFactorBy :: (Particle -> Bool) -> Fuel -> HeatFluxDensity Double
> rxFactorBy f fuel = totalLoadBy f fuel
>                   * totalHeatBy f fuel
>                   * totalEtaSBy f fuel
>                   * gamma fuel
>                   / (1 *~ second)
 
> gamma
>  , slopeK
>  , windB
>  , windK
>  , windE
>  , c
>  , e
>  , liveExtinctionFactor
>     :: Fuel -> Dimensionless Double

> gamma fuel = gammaMax * (ratio fuel ** aa) * exp (aa * (_1 - ratio fuel))
>     where gammaMax = sigma15 / ((495 *~ one) + (0.0594 *~ one) * sigma15)
>           sigma15  = (sigma fuel) ** (1.5 *~ one)
>           aa       = (133 *~ one) / (sigma fuel ** (0.7913 *~ one))

> ratio fuel = beta fuel / betaOpt
>     where betaOpt  = (3.348 *~ one) / (sigma fuel ** (0.8189 *~ one))

> propagatingFluxRatio fuel = toUnits pfr (meter^neg2 * second^pos2)
>     where pfr = exp (s1 * b) / s2
>           s1  = (0.792 *~ one) + (0.681 *~ one) * sqrt (sigma fuel)
>           b   = beta fuel + (0.1*~one)
>           s2  = (192 *~ one) + (0.2595 *~ one) * sigma fuel

> toUnits v u = (v /~ one) *~ u

> slopeK fuel = (5.275 *~ one) * (beta fuel ** ((-0.3)*~one))

> windB fuel = (0.02526 *~ one) * (sigma fuel ** (0.54*~one))

> c fuel = (7.47*~one) * exp (((-0.133)*~one) * (sigma fuel) ** (0.55*~one))

> e fuel = (0.715 *~ one) * exp (((-0.000359) *~ one) * sigma fuel)

> windK fuel = c fuel * (ratio fuel ** ( _0 - (e fuel)))

> windE fuel = (ratio fuel ** e fuel) / c fuel

> liveExtinctionFactor fuel
>     | nonZero (fineLive fuel) lbSqFt = (2.9*~one)
>                                      * fineDead fuel / fineLive fuel
>     | otherwise                      = (0 *~ one)

> hasLiveFuel fuel = nonZero (totalLoadBy isAlive fuel) lbSqFt

> fineLive, fineDead :: Fuel -> FuelLoad
> fineLive fuel =
>     if hasLiveFuel fuel
>     then overFilteredParticles 
>          (\p -> load p * exp (((-500)*~perFoot)/ savr p))
>          isAlive
>          fuel
>     else (0 *~ lbSqFt)

Nota: el código original no calcula fineDead si no hay partículas vivas en el
combustible. Lo cual no tiene mucho sentido, ¿no? Intentamos ser fieles a la
implementación original

> fineDead fuel =
>     if hasLiveFuel fuel
>     then overFilteredParticles 
>          (\p -> load p * sigmaFactor p)
>          (not.isAlive)
>          fuel
>     else (0 *~ lbSqFt)


A continuación las ecuaciones de los parámetros que dependen de la humedad del
combustible

> data TimeLag = TimeLag {
>       d1hr    :: Moisture
>     , d10hr   :: Moisture
>     , d100hr  :: Moisture
>     , d1000hr :: Moisture
>     , herb    :: Moisture
>     , wood    :: Moisture
> } deriving (Show)

> mkTimeLag l =
>     case length l of
>          6  -> TimeLag (ts!!0) (ts!!1) (ts!!2) (ts!!3) (ts!!4) (ts!!5)
>          _  -> error "invalid list length"
>     where ts = map toMoisture l

> toMoisture :: Double -> Moisture
> toMoisture v
>     | v>=0 && v<=1 = (v *~ one)
>     | otherwise    = error "moisture must be between 0 a 1"
            

> particleMoisture :: Particle -> TimeLag -> Moisture
> particleMoisture p =
>    case type_ p of
>      Herb -> herb
>      Wood -> wood
>      Dead -> [d1hr, d1hr, d10hr, d10hr, d100hr, d100hr] !! (sizeClass p)


> totalMoistureBy :: (Particle -> Bool) -> TimeLag -> Fuel -> Moisture
> totalMoistureBy f tl fuel =
>    overFilteredParticles
>    (\p-> (areaWeight fuel p) * (particleMoisture p tl))
>    f
>    fuel

> type IsFuelAlive = Bool

> moisture, extinctionMoisture, etaM ::
>   IsFuelAlive -> TimeLag -> Fuel -> Moisture

> moisture True = totalMoistureBy isAlive
> moisture False = totalMoistureBy (not.isAlive)

> extinctionMoisture True tl fuel
>    | hasLiveParticles fuel = liveExtinctionFactor fuel
>                            * (_1 - (fdmois tl fuel)/(mext fuel))
>                            - (0.226 *~ one)
>    | otherwise             = _0
> extinctionMoisture False tl fuel = mext fuel

> hasLiveParticles :: Fuel -> Bool
> hasLiveParticles = any isAlive . particles

> etaM alive tl fuel
>     | nonZero mext one &&
>       moist < mext  = etaM'
>     | otherwise     = _0
>   where mext  = extinctionMoisture alive tl fuel
>         moist = moisture alive tl fuel 
>         ratio = moist / mext
>         etaM' = _1
>               - (2.59 *~ one) * ratio
>               + (5.11 *~ one) * (ratio ** _2)
>               - (3.52 *~ one) * (ratio ** _3)
>      

> fdmois :: TimeLag -> Fuel -> Moisture
> fdmois tl fuel
>    | hasLiveParticles fuel &&
>      nonZero (fineDead fuel) lbSqFt = (wfmd tl fuel) / fineDead fuel
>    | otherwise                      = _0

> wfmd :: TimeLag -> Fuel -> FuelLoad
> wfmd tl fuel =
>   overFilteredParticles
>   (\p -> (particleMoisture p tl) * sigmaFactor p * load p)
>   (not.isAlive)
>   fuel

> rbQig :: TimeLag -> Fuel -> Density
> rbQig tl fuel = (bulkDensity fuel) * (overParticles qig fuel)
>     where qig p = ((250 *~ one) + (1116 *~ one) * (particleMoisture  p tl))
>                 * (areaWeight fuel p)
>                 * (totalAreaWtBy (\p' -> isAlive p == isAlive p') fuel)
>                 * sigmaFactor p

> reactionIntensity :: TimeLag -> Fuel -> HeatFluxDensity Double
> reactionIntensity tl fuel
>     = (rxFactorBy isAlive fuel)       * (etaM True tl fuel)
>     + (rxFactorBy (not.isAlive) fuel) * (etaM False tl fuel)
>

> heatPerUnitArea :: TimeLag -> Fuel -> HeatPerUnitArea
> heatPerUnitArea tl fuel = (reactionIntensity tl fuel) * residenceTime fuel

> spread0 :: TimeLag -> Fuel -> Velocity Double

> spread0 tl fuel
>    | nonZero (rbQig tl fuel) (gram/meter^pos3) = reactionIntensity tl fuel
>                                                * propagatingFluxRatio fuel
>                                                / rbQig tl fuel
>    | otherwise                                 = 0 *~ (meter/second)
