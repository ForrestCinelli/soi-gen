module PlanetaryFeature exposing 
    ( PlanetaryFeature(..)
    , PlanetaryFeatureChoice(..)
    , planetaryFeatureChoices
    , TerrestrialPlanet(..)
    , GiantPlanet(..)
    , DerelictStationOrigin(..)
    , StarshipGraveyardOrigin(..)
    , randomInner
    , randomHabitable
    , randomOuter
    , showPlanet
    , showBody
    , showGravity
    , showTemperature
    , showAtmosphere
    , showAtmosphereComposition
    , showGasBody
    , showGasGravity
    , showGraveyardOrigin
    , showDerelictStationOrigin
    )

import Random
import BasicTypes exposing (Zone(..))

type PlanetaryFeature
    = RockyPlanet TerrestrialPlanet
    | GasGiant GiantPlanet
    | AsteroidBelt
    | AsteroidCluster
    | DustCloud
    | GravityRiptide
    | RadiationBurst
    | SolarFlare
    | DerelictStation DerelictStationOrigin
    | StarshipGraveyard StarshipGraveyardOrigin

type PlanetaryFeatureChoice
    = RockyFeature
    | GasFeature
    | AsteroidBeltFeature
    | AsteroidClusterFeature
    | DustCloudFeature
    | GravityRiptideFeature
    | RadiationBurstFeature
    | SolarFlareFeature
    | DerelictStationFeature
    | StarshipGraveyardFeature
planetaryFeatureChoices = 
    [ RockyFeature
    , GasFeature
    , AsteroidBeltFeature
    , AsteroidClusterFeature
    , DustCloudFeature
    , GravityRiptideFeature
    , RadiationBurstFeature
    , SolarFlareFeature
    , DerelictStationFeature
    , StarshipGraveyardFeature
    ]

type TerrestrialPlanet = TerrestrialPlanet PlanetBody PlanetGravity Atmosphere Temperature (List OrbitalFeature)
type GiantPlanet = GiantPlanet GasBody GasGravity (List OrbitalFeature)

type GasBody = Dwarf | Giant | Massive -- massive > giant
gasBodies = [Dwarf, Giant, Massive]
type GasGravity = Weak | Strong | Powerful | Titanic
gasGravities = [Weak, Strong, Powerful, Titanic]

type PlanetBody = LowMass | Small | SmallDense | Large | LargeDense | Vast
planetBodies = [LowMass, Small, SmallDense, Large, LargeDense, Vast]
type PlanetGravity = Low | Normal | High
planetGravities = [Low, Normal, High]

type Temperature = Burning | Hot | Temperate | Cold | Ice
temperatures = [Burning, Hot, Temperate, Cold, Ice]

type Atmosphere = None | Thin AtmosphereComposition | Moderate AtmosphereComposition | Heavy AtmosphereComposition
type AtmosphereChoice = NoAtm | ThinAtm | ModerateAtm | HeavyAtm
atmosphereChoices = [NoAtm, ThinAtm, ModerateAtm, HeavyAtm]
type AtmosphereComposition = Deadly | Corrosive | Toxic | Tainted | Pure
atmosphereCompositions = [Deadly, Corrosive, Toxic, Tainted, Pure]

type StarshipGraveyardOrigin = CrushedFleet | FleetEngagement | LostExplorers | PlunderedConvoy | Skirmish | Unknown
starshipGraveyardOrigins = [CrushedFleet, FleetEngagement, LostExplorers, PlunderedConvoy, Skirmish, Unknown]
type DerelictStationOrigin
    = EgarianVoidMaze
    | EldarOrrery 
    | EldarGate 
    | OrkRok 
    | DefenseStation 
    | MonitorStation 
    | StryxisCollection 
    | XenosDefenseStation 
    | XenosMonitorStation
derelictStationOrigins = 
    [ EgarianVoidMaze
    , EldarOrrery 
    , EldarGate 
    , OrkRok 
    , DefenseStation 
    , MonitorStation 
    , StryxisCollection 
    , XenosDefenseStation 
    , XenosMonitorStation
    ]

showPlanet: PlanetaryFeature -> String
showPlanet p = case p of
    RockyPlanet deets -> "Planet"
    GasGiant deets -> "Gas Giant"
    AsteroidBelt -> "Asteroid Belt"
    AsteroidCluster -> "Asteroid Cluster"
    DustCloud -> "Dust Cloud"

    GravityRiptide -> "Gravity Riptide"
    RadiationBurst -> "Radiation Burst"
    SolarFlare -> "Solar Flare"
    DerelictStation o -> "Derelict Station"
    StarshipGraveyard o -> "Starship Graveyard"

showBody: PlanetBody -> String
showBody b = case b of 
    LowMass -> "Low Mass"
    Small -> "Small"
    SmallDense -> "Small but dense"
    Large -> "Large"
    LargeDense -> "Large and dense"
    Vast -> "Vast"

showGravity: PlanetGravity -> String
showGravity g = case g of
    Low -> "low gravity"
    Normal -> "terra-like gravity"
    High -> "high gravity"

showTemperature: Temperature -> String
showTemperature t = case t of 
    Burning -> "Burning"
    Hot -> "Hot"
    Temperate -> "Temperate"
    Cold -> "Cold"
    Ice -> "Frozen"

showAtmosphere: Atmosphere -> String
showAtmosphere a = case a of 
    None -> "No atmosphere"
    Thin c -> "Thin, " ++ (showAtmosphereComposition c) ++ " atmosphere"
    Moderate c -> "Moderately thick, " ++ (showAtmosphereComposition c) ++ " atmosphere"
    Heavy c -> "Heavy, " ++ (showAtmosphereComposition c) ++ " atmosphere"

showAtmosphereComposition: AtmosphereComposition -> String
showAtmosphereComposition c = case c of 
    Deadly -> "Deadly"
    Corrosive -> "Corrosive"
    Toxic -> "Toxic"
    Tainted -> "Tainted"
    Pure -> "Pure"

showGasBody: GasBody -> String
showGasBody b = case b of 
    Dwarf -> "Gas dwarf"
    Giant -> "Giant gas planet"
    Massive -> "Massive gas giant"

showGasGravity: GasGravity -> String
showGasGravity g = case g of 
    Weak -> "weak gravity"
    Strong -> "strong gravity"
    Powerful -> "powerful gravity"
    Titanic -> "titanic gravity"

showGraveyardOrigin: StarshipGraveyardOrigin -> String
showGraveyardOrigin o = "Origin: " ++ (case o of
        CrushedFleet -> "Crushed Defence Force/Routed Invasion"
        FleetEngagement -> "Fleet Engagement"
        LostExplorers -> "Lost Explorers"
        PlunderedConvoy -> "Plundered Convoy"
        Skirmish -> "Skirmish"
        Unknown -> "Unknown Provenance"
    )

showDerelictStationOrigin: DerelictStationOrigin -> String
showDerelictStationOrigin o = "Origin: " ++ (case o of
        EgarianVoidMaze -> "Egarian Void-maze"
        EldarOrrery -> "Eldar Orrery" 
        EldarGate -> "Eldar Gate" 
        OrkRok -> "Ork Rok" 
        DefenseStation -> "STC Defense Station" 
        MonitorStation -> "STC Monitor Station" 
        StryxisCollection -> "Syryxis Collection"
        XenosDefenseStation -> "Xenos Defense Station" 
        XenosMonitorStation -> "Xenos Monitor Station"
    )

type OrbitalFeature = Moon

randomInner: Random.Generator PlanetaryFeature
randomInner = (Random.int 21 100) |> Random.andThen (\roll -> 
        if roll <= 29      then Random.constant AsteroidCluster
        else if roll <= 41 then Random.constant DustCloud
        else if roll <= 45 then Random.map (\gp -> GasGiant gp) (randomGas) -- todo consider increasing the probability of GLASS RAIN GAS GIANTS
        else if roll <= 56 then Random.constant GravityRiptide
        else if roll <= 76 then Random.map (\tp -> RockyPlanet tp) (randomRocky Inner)
        else if roll <= 89 then Random.constant RadiationBurst
        else                    Random.constant SolarFlare
    )

--in habitable zone, not necessarily habitable
randomHabitable: Random.Generator PlanetaryFeature
randomHabitable = (Random.int 21 100) |> Random.andThen (\roll ->
        if roll <= 30      then Random.constant AsteroidBelt
        else if roll <= 41 then Random.constant AsteroidCluster
        else if roll <= 47 then Random.map (\o -> DerelictStation o) randomStationOrigin
        else if roll <= 58 then Random.constant DustCloud
        else if roll <= 64 then Random.constant GravityRiptide
        else if roll <= 93 then Random.map (\tp -> RockyPlanet tp) (randomRocky Habitable)
        else                    Random.map (\o -> StarshipGraveyard o) randomGraveyardOrigin
    )

randomOuter: Random.Generator PlanetaryFeature
randomOuter =  (Random.int 21 100) |> Random.andThen (\roll ->
        if roll <= 29 then      Random.constant AsteroidBelt
        else if roll <= 40 then Random.constant AsteroidCluster
        else if roll <= 46 then Random.map (\o -> DerelictStation o) randomStationOrigin
        else if roll <= 55 then Random.constant DustCloud
        else if roll <= 73 then Random.map (\gp -> GasGiant gp) (randomGas)
        else if roll <= 80 then Random.constant GravityRiptide
        else if roll <= 93 then Random.map (\tp -> RockyPlanet tp) (randomRocky Outer)
        else                    Random.map (\o -> StarshipGraveyard o) randomGraveyardOrigin
    )



randomRocky: Zone -> Random.Generator TerrestrialPlanet
randomRocky zone =
    randomBody |> Random.andThen (\body -> 
        randomGravity body |> Random.andThen (\gravity ->
            randomAtmosphere gravity |> Random.andThen (\atmosphere -> 
                randomTemperature zone atmosphere |> Random.andThen (\temperature -> 
                    Random.constant (TerrestrialPlanet body gravity atmosphere temperature [])))))


randomBody: Random.Generator PlanetBody
randomBody = Random.map (\roll ->
        if roll <= 1      then LowMass
        else if roll <= 3 then Small
        else if roll <= 4 then SmallDense
        else if roll <= 7 then Large
        else if roll <= 8 then LargeDense
        else                   Vast
    ) (Random.int 1 10)

randomGravity: PlanetBody -> Random.Generator PlanetGravity
randomGravity body = Random.map (\base ->
    let 
        modifier = 
            case body of 
                LowMass -> -7
                Small -> -5
                SmallDense -> 0
                Large -> 0
                LargeDense -> 5
                Vast -> 4
        
        roll = base + modifier
    in 
        if roll <= 2      then Low
        else if roll <= 8 then Normal
        else                   High
    ) (Random.int 1 10)

randomAtmosphere: PlanetGravity -> Random.Generator Atmosphere
randomAtmosphere g = (Random.int 1 10) |> Random.andThen (\base -> 
        let
            adjustment = 
                case g of 
                    Low -> -2
                    Normal -> 0
                    High -> 1
            roll = base + adjustment
            ac = randomAtmosphericComposition
        in 
            if roll <= 1       then Random.constant None
            else if roll <= 4  then ac |> Random.map (\c -> Thin c) 
            else if roll <= 9  then ac |> Random.map (\c -> Moderate c)
            else                    ac |> Random.map (\c -> Heavy c)
    )

randomAtmosphericComposition: Random.Generator AtmosphereComposition
randomAtmosphericComposition = Random.map (\roll -> 
        if roll <= 1      then Deadly
        else if roll <= 2 then Corrosive
        else if roll <= 5 then Toxic
        else if roll <= 7 then Tainted
        else                   Pure
    ) (Random.int 1 10)

randomTemperature: Zone -> Atmosphere -> Random.Generator Temperature
randomTemperature zone atmosphere = 
    Random.map (\base -> 
        let
            adjustment = 
                case (zone, atmosphere) of 
                    (Inner, None) -> -666 -- always burning
                    (Inner, x) -> -6
                    (Habitable, None) -> 666 -- todo randomize
                    (Habitable, x) -> 0
                    (Outer, None) -> 666 -- always frozen
                    (Outer, x) -> 6
            roll = base + adjustment
        in 
            if roll <= 0       then Burning
            else if roll <= 3  then Hot
            else if roll <= 7  then Temperate
            else if roll <= 10 then Cold
            else                    Ice
    ) (Random.int 1 10)

randomGas: Random.Generator GiantPlanet
randomGas = randomGasBody |> Random.andThen (\body ->
    randomGasGravity body |> Random.andThen (\gravity ->
        Random.constant (GiantPlanet body gravity [])))

randomGasBody: Random.Generator GasBody
randomGasBody = Random.map (\roll -> 
        if roll <= 2      then Dwarf 
        else if roll <= 8 then Giant
        else                   Massive -- todo 1d10 to decide if it's actually a brown dwarf or whatever
    ) (Random.int 1 10)

randomGasGravity: GasBody -> Random.Generator GasGravity
randomGasGravity body = Random.map (\base ->
        let 
            adjustment = 
                case body of
                    Dwarf -> -5
                    Giant -> 0
                    Massive -> 3
            roll = base + adjustment
        in
            if roll <= 2      then Weak
            else if roll <= 6 then Strong
            else if roll <= 9 then Powerful
            else                   Titanic
    ) (Random.int 1 10)

randomGraveyardOrigin: Random.Generator StarshipGraveyardOrigin
randomGraveyardOrigin = (Random.int 1 100) |> Random.andThen (\roll ->
        if roll <= 15 then      Random.constant CrushedFleet
        else if roll <= 20 then Random.constant FleetEngagement
        else if roll <= 35 then Random.constant LostExplorers
        else if roll <= 65 then Random.constant PlunderedConvoy
        else if roll <= 90 then Random.constant Skirmish
        else                    Random.constant Unknown
    )

randomStationOrigin: Random.Generator DerelictStationOrigin
randomStationOrigin = (Random.int 1 100) |> Random.andThen (\roll ->
        if roll <= 10 then      Random.constant EgarianVoidMaze
        else if roll <= 20 then Random.constant EldarOrrery
        else if roll <= 25 then Random.constant EldarGate
        else if roll <= 40 then Random.constant OrkRok
        else if roll <= 50 then Random.constant DefenseStation
        else if roll <= 65 then Random.constant MonitorStation
        else if roll <= 76 then Random.constant StryxisCollection
        else if roll <= 85 then Random.constant XenosDefenseStation
        else                    Random.constant XenosMonitorStation
    )