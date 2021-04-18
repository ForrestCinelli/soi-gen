--todo:
    --generates when you press g
    --use probability distributions to handle the 01-20 case where a feature is "None"
    --orbital features
        --type 
        --generator
        --view
    --names
    --make planets and features match book rprecisely

module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, attribute)
import List exposing (head, length, tail, map, concatMap)
import Maybe
import Random
import Random.Extra
import Tuple exposing (first, second)

main = Browser.document 
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = \modl -> { title = "SOI Gen", body = [ view modl ] }
    }

type alias Model = { star: Star, feature: SystemFeature, innerZone: List PlanetaryFeature, habitableZone: List PlanetaryFeature, outerZone: List PlanetaryFeature }

----

type Star = M | G | F | B | VII | Binary Star Star
stars = [B, F, G, M, VII]

maximum: Star -> Star -> Maybe Star
maximum s t = case (s, t) of 
    ((Binary ss st), _) -> Maybe.andThen (maximum t) (maximum ss st)
    (_, (Binary ts tt)) -> Maybe.andThen (maximum s) (maximum ts tt)
    (_, _) -> maxFinder stars s t

--maxFinder: Maybe (List Star) -> Star -> Star -> Maybe Star
--maxFinder maybeStars s t = case maybeStars of
--    Just (cstar :: starList) ->
--        if (s == cstar || t == cstar) then Just cstar
--        else if (s == t)              then Just s
--        else                               maxFinder (tail starList) s t
--    Just [] -> Nothing
--    Nothing -> Nothing

maxFinder starss s t = List.foldl (
    \star -> \maybe -> case maybe of 
      Just x -> Just x
      Nothing -> if (star == s || star == t) then (Just star) else Nothing
  ) Nothing starss

type SystemFeature = Bountiful | GravityTides | Haven | IllOmened | PirateDen | RuinedEmpire | Starfarers | StellarAnomaly | WarpStasis | WarpTurbulence

type Zone = Inner | Habitable | Outer

regionSizes: Star -> { inner: Int, habitable: Int, outer: Int }
regionSizes star = case star of 
    Binary s t -> case (maximum s t) of 
        Just m -> regionSizes m
        Nothing -> { inner = 0, habitable = 0, outer = 3 } -- todo this is for debugging
    VII -> { inner = 0, habitable = 1, outer = 5 }
    M -> { inner = 3, habitable = 3, outer = 5 }
    G -> { inner = 1, habitable = 5, outer = 3 }
    F -> { inner = 3, habitable = 3, outer = 3 }
    B -> { inner = 5, habitable = 1, outer = 3 }

showStar: Star -> String
showStar star = case star of 
    M -> "Red Dwarf"
    G -> "Yellow Dwarf"
    F -> "Yellow-White Star"
    B -> "Blue Giant"
    VII -> "White Dwarf"
    Binary s t -> (showStar s) ++ " / " ++  (showStar t) ++ " pair"

type PlanetaryFeature
    = RockyPlanet TerrestrialPlanet
    | GasGiant GiantPlanet
    | AsteroidBelt 
    | AsteroidCluster 
    | DustCloud 
    | GravityRiptide 
    | RadiationBurst 
    | SolarFlare 
    | DerelictStation DerelicitStationOrigin 
    | StarshipGraveyard StarshipGraveyardOrigin
type TerrestrialPlanet = TerrestrialPlanet PlanetBody PlanetGravity Atmosphere Temperature (List OrbitalFeature)
type GiantPlanet = GiantPlanet GasBody GasGravity (List OrbitalFeature)

type GasBody = Dwarf | Giant | Massive -- massive > giant
type GasGravity = Weak | Strong | Powerful | Titanic

type PlanetBody = LowMass | Small | SmallDense | Large | LargeDense | Vast
type PlanetGravity = Low | Normal | High

type Temperature = Burning | Hot | Temperate | Cold | Ice

type Atmosphere = None | Thin AtmosphereComposition | Moderate AtmosphereComposition | Heavy AtmosphereComposition
type AtmosphereComposition = Deadly | Corrosive | Toxic | Tainted | Pure

type StarshipGraveyardOrigin = CrushedFleet | FleetEngagement | LostExplorers | PlunderedConvoy | Skirmish | Unknown
type DerelicitStationOrigin = EgarianVoidMaze | EldarOrrery | EldarGate | OrkRok | DefenseStation | MonitorStation | StryxisCollection | XenosDefenseStation | XenosMonitorStation


showSystemFeature: SystemFeature -> String
showSystemFeature sf = case sf of 
    Bountiful -> "Bountiful" 
    GravityTides -> "Gravity Tides" 
    Haven -> "Haven" 
    IllOmened -> "Ill-Omened" 
    PirateDen -> "Pirate Den" 
    RuinedEmpire -> "Ruined Empire" 
    Starfarers -> "Starfarers" 
    StellarAnomaly -> "Stellar Anomaly" 
    WarpStasis -> "Warp Stasis"
    WarpTurbulence -> "Warp Turbulence"

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

showDerelictStationOrigin: DerelicitStationOrigin -> String
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

init: () -> (Model, Cmd Msg)
init _ = ({ star = M, feature = IllOmened, innerZone = [], habitableZone = [], outerZone = [] }, Random.generate NewSystem randomModel)

----

type Msg = Generate | NewSystem Model

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    NewSystem s -> (s, Cmd.none)
    Generate ->
        ( model
        , Random.generate NewSystem randomModel
        )

randomModel: Random.Generator Model
randomModel = Random.Extra.andThen2 planetsForStar
                                    randomSystemFeature
                                    randomStar

randomStar: Random.Generator Star
randomStar = (Random.int 1 10) |> Random.andThen (\roll ->
    if roll <= 1      then Random.constant B
    else if roll <= 4 then Random.constant F
    else if roll <= 7 then Random.constant G
    else if roll <= 8 then Random.constant M
    else if roll <= 9 then Random.constant VII
    else                   Random.map2 (\s -> \t -> Binary s t) randomNonbinaryStar randomNonbinaryStar -- todo book says they should match 7/10 times
    )

randomNonbinaryStar: Random.Generator Star
randomNonbinaryStar = Random.uniform M [ G, F, B, VII ]

randomSystemFeature: Random.Generator SystemFeature
randomSystemFeature = Random.map (\roll ->
    if roll <= 1      then Bountiful
    else if roll <= 2 then GravityTides
    else if roll <= 3 then Haven
    else if roll <= 4 then IllOmened
    else if roll <= 5 then PirateDen
    else if roll <= 6 then RuinedEmpire
    else if roll <= 7 then Starfarers
    else if roll <= 8 then StellarAnomaly
    else if roll <= 9 then WarpStasis
    else                   WarpTurbulence
    ) (Random.int 1 10)

planetsForStar feature star = Random.map3 (\i -> \h -> \o -> Model star feature i h o)
                                          (Random.list (regionSizes star).inner randomInner)
                                          (Random.list (regionSizes star).habitable randomHabitable)
                                          (Random.list (regionSizes star).outer randomOuter)


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

randomStationOrigin: Random.Generator DerelicitStationOrigin
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

----

view: Model -> Html Msg
view model = div []
    [ system model
    ]

system: Model -> Html Msg
system {star, feature, innerZone, habitableZone, outerZone} =
    div planetContainerStyle
        ((starView star feature) :: (innerView innerZone) ++ (habitableView habitableZone) ++ (outerView outerZone))

starView: Star -> SystemFeature -> Html Msg
starView star feature = div starStyle 
    [ div starContainerStyle [ img (starImg star) [] ]
    , div [ style "text-align" "center", style "padding" "1vw 0.1vw 0vw 0.1vw", style "font-size" "24px" ] [ text (showStar star) ]
    , div ((style "padding-bottom" "0.5vw") :: (style "text-align" "center") :: detailStyle) [ text (showSystemFeature feature) ]
    , generateButton
    ]

starImg star = (case star of
    Binary _ _ -> attribute "src" "https://soi-gen.herokuapp.com/Binary.jpg"
    VII -> attribute "src" "https://soi-gen.herokuapp.com/WhiteDwarf.jpg"
    M -> attribute "src" "https://soi-gen.herokuapp.com/RedDwarf2.jpg"
    G -> attribute "src" "https://soi-gen.herokuapp.com/YellowDwarf.jpg"
    F -> attribute "src" "https://soi-gen.herokuapp.com/YellowWhiteDwarf.jpg"
    B -> attribute "src" "https://soi-gen.herokuapp.com/BlueGiant2.jpg"
    ) :: 
    [ style "padding" "0px 0px 0px 0px"
    , style "max-width" "15vw"
    , style "max-height" "20vh"
    , style "margin" "0"
    ]

starStyle: List (Html.Attribute msg)
starStyle = 
    [ style "font-weight" "bold"
    , style "grid-row-start" "1"
    , style "grid-column-start" "1"
    , style "grid-row-end" "3"
    , style "padding" "0px 0px 0px 0px"
    , style "margin" "0"
    , style "width" "15vw"
    ]
starContainerStyle: List(Html.Attribute msg)
starContainerStyle = 
    [ style "width" "15vw"
    , style "background-color" "black"
    , style "padding" "0px 0px 0px 0px"
    , style "display" "flex"
    , style "justify-content" "center"
    ]

planetContainerStyle: List (Html.Attribute msg)
planetContainerStyle = 
    [ --style "display" "flex"
    --, style "align-items" "stretch"
      style "display" "grid"
    , style "gap" "0px 0px"
    , style "grid-template-columns" "15vw" -- rest auto
    , style "width" "100%"
    , style "margin" "0"
    ]

innerView: List PlanetaryFeature -> List (Html Msg)
innerView = concatMap (
        \p -> 
            let pv = planetView "#EEB0B0" p 
            in [ first pv, second pv ]
    )

habitableView: List PlanetaryFeature -> List (Html Msg)
habitableView = concatMap (
        \p -> 
            let pv = planetView "#B0EEB0" p 
            in [ first pv, second pv ]
    )

outerView: List PlanetaryFeature -> List (Html Msg)
outerView = concatMap (
        \p -> 
            let pv = planetView "#B0B0EE" p 
            in [ first pv, second pv ]
    )
--<i> for names
planetStyle: String -> List (Html.Attribute msg)
planetStyle color = 
    [ --style "width" "2000px" -- todo there has to be a better way to get them to just have no space in between

     style "background-color" color
    ]

planetView: String -> PlanetaryFeature -> (Html Msg, Html Msg)
planetView color planet = case planet of
    DerelictStation o ->
        ( p ((style "background-color" color) :: headerStyle) [ text (showPlanet planet) ]
        , p ((style "background-color" color) :: detailStyle) [ text (showDerelictStationOrigin o) ]
        )
    StarshipGraveyard o -> 
        ( p ((style "background-color" color) :: headerStyle) [ text (showPlanet planet) ]
        , p ((style "background-color" color) :: detailStyle) [ text (showGraveyardOrigin o) ]
        )
    RockyPlanet (TerrestrialPlanet body gravity atmosphere temperature orbitalFeatures) -> 
        ( p ((style "background-color" color) :: headerStyle) [ text (showPlanet planet) ]
        , div [ row 2, style "background-color" color ] 
            [ p detailStyle [ text ((showBody body) ++ " with " ++ (showGravity gravity)) ]
            , p detailStyle [ text ((showTemperature temperature) ++ " world") ]
            , p detailStyle [ text (showAtmosphere atmosphere) ]
            ]
        )
    GasGiant (GiantPlanet body gravity orbitalFeatures) ->
        ( p ((style "background-color" color) :: headerStyle) [ text (showPlanet planet) ]
        , p ((style "background-color" color) :: detailStyle) [ text ((showGasBody body) ++ " with " ++ (showGasGravity gravity)) ]
        )
    x -> 
        ( p ((style "background-color" color) :: headerStyle) [ text (showPlanet x) ]
        , div [ row 2, style "background-color" color ] []
        )

row x = style "grid-row-start" (String.fromInt x)

detailStyle: List (Html.Attribute msg)
detailStyle = 
    [ style "font-weight" "normal"
    , style "font-style" "italic"
    , style "margin" "0"
    , style "font-size" "small"
    , style "padding-top" "5%"
    , style "padding-left" "5%"
    , style "padding-right" "5%"
    , style "padding-bottom" "5%"
    , row 2
    ]

headerStyle: List(Html.Attribute msg)
headerStyle = 
    [ style "font-weight" "bold"
    , style "font-size" "large"
    , row 1
    , style "margin" "0"
    , style "padding-top" "5%"
    , style "padding-left" "5%"
    , style "padding-right" "5%"
    , style "padding-bottom" "5%"
    ]

--

generateButton = div []
    [ button ((onClick Generate) :: buttonStyle)
             [ text "Generate" ]
    ]

buttonStyle: List (Html.Attribute msg)
buttonStyle = 
    [ style "width" "15vw"
    , style "height" "3vh"
    , style "min-height" "28px"
    , style "margin-top" "10px"
    , style "font-size" "18px"
    , style "border" "none"
    --, style "background-color" "#397cd5"
    , style "background-color" "#7171e0"
    , style "color" "white"
    ]
