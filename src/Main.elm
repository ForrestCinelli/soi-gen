--todo:
    --star photos
    --planet characteristics
        --generator
        --view
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
import List exposing (head, length, tail, map)
import Maybe
import Random

main = Browser.element { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }

type alias Model = { star: Star, innerZone: List PlanetaryFeature, habitableZone: List PlanetaryFeature, outerZone: List PlanetaryFeature }

----

type Star = M | G | F | B | VII
stars = [M, G, F, B, VII]

regionSizes: Star -> { inner: Int, habitable: Int, outer: Int }
regionSizes star = case star of 
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

--RockyPlanet (List (Maybe OrbitalFeature)) | GasGiant (List (Maybe PlanetaryFeature)) | AsteroidBelt | AsteroidCluster
type PlanetaryFeature = RockyPlanet | GasGiant | AsteroidBelt | AsteroidCluster | DustCloud | GravityRiptide | RadiationBurst | SolarFlare | DerelictStation DerelicitStationOrigin | StarshipGraveyard StarshipGraveyardOrigin
type TerrestrialPlanet = TerrestrialPlanet PlanetBody PlanetGravity Atmosphere Temperature (List OrbitalFeature)
type GiantPlanet = GiantPlanet GasBody GasGravity (List OrbitalFeature)

type GasBody = Dwarf | Giant | Massive -- massive > giant
type GasGravity = Weak | String | Powerful | Titanic

type PlanetBody = LowMass | Small | SmallDense | Large | LargeDense | Vast
type PlanetGravity = Low | Normal | High

type Temperature = Burning | Hot | Temperate | Cold | Ice

type Atmosphere = None | Thin AtmosphereComposition | Moderate AtmosphereComposition | Heavy AtmosphereComposition
type AtmosphereComposition = Deadly | Corrosive | Toxic | Tainted | Pure

type StarshipGraveyardOrigin = CrushedFleet | FleetEngagement | LostExplorers | PlunderedConvoy | Skirmish | Unknown
type DerelicitStationOrigin = EgarianVoidMaze | EldarOrrery | EldarGate | OrkRok | DefenseStation | MonitorStation | StryxisCollection | XenosDefenseStation | XenosMonitorStation

tmpPlanet = TerrestrialPlanet LowMass Low None Burning []
tmpGiant = GiantPlanet Dwarf Weak []

showPlanet: PlanetaryFeature -> String
showPlanet p = case p of
    RockyPlanet -> "Planet"
    GasGiant -> "Gas Giant"
    AsteroidBelt -> "Asteroid Belt"
    AsteroidCluster -> "Asteroid Cluster"
    DustCloud -> "Dust Cloud"

    GravityRiptide -> "Gravity Riptide"
    RadiationBurst -> "Radiation Burst"
    SolarFlare -> "Solar Flare"
    DerelictStation o -> "Derelict Station"
    StarshipGraveyard o -> "Starship Graveyard"

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
init _ = ({ star = M, innerZone = [], habitableZone = [], outerZone = [] }, Random.generate NewSystem randomModel)

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
randomModel = randomStar |> Random.andThen planetsForStar

randomStar: Random.Generator Star
randomStar = Random.uniform M [ G, F, B ]

planetsForStar star = Random.map3 (\i -> \h -> \o -> Model star i h o)
                                  (Random.list (regionSizes star).inner randomInner)
                                  (Random.list (regionSizes star).habitable randomHabitable)
                                  (Random.list (regionSizes star).outer randomOuter)


randomInner: Random.Generator PlanetaryFeature
randomInner = (Random.int 21 100) |> Random.andThen (\roll -> 
        if roll <= 29      then Random.constant AsteroidCluster
        else if roll <= 41 then Random.constant DustCloud
        else if roll <= 45 then randomGas -- todo consider increasing the probability of GLASS RAIN GAS GIANTS
        else if roll <= 56 then Random.constant GravityRiptide
        else if roll <= 76 then randomRocky
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
        else if roll <= 93 then randomRocky
        else                    Random.map (\o -> StarshipGraveyard o) randomGraveyardOrigin
    )

randomOuter: Random.Generator PlanetaryFeature
randomOuter =  (Random.int 21 100) |> Random.andThen (\roll ->
        if roll <= 29 then      Random.constant AsteroidBelt
        else if roll <= 40 then Random.constant AsteroidCluster
        else if roll <= 46 then Random.map (\o -> DerelictStation o) randomStationOrigin
        else if roll <= 55 then Random.constant DustCloud
        else if roll <= 73 then randomGas
        else if roll <= 80 then Random.constant GravityRiptide
        else if roll <= 93 then randomRocky
        else                    Random.map (\o -> StarshipGraveyard o) randomGraveyardOrigin
    )



randomRocky: Random.Generator PlanetaryFeature--TerrestrialPlanet
randomRocky = Random.constant RockyPlanet

randomGas: Random.Generator PlanetaryFeature--GiantPlanet
randomGas = Random.constant GasGiant

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
    , generateButton
    ]

system: Model -> Html Msg
system {star, innerZone, habitableZone, outerZone} =
    div planetContainerStyle
        ((starView star) :: (innerView innerZone) ++ (habitableView habitableZone) ++ (outerView outerZone))

starView: Star -> Html Msg
starView star = div starStyle 
    [ img (starImg star) []
    , (text (showStar star))
    ]

starImg star = (case star of
    VII -> attribute "src" "file:///C:/Users/Forre/elm/soi-gen/WhiteDwarf.jpg"
    M -> attribute "src" "file:///C:/Users/Forre/elm/soi-gen/RedDwarf.jpg"
    G -> attribute "src" "file:///C:/Users/Forre/elm/soi-gen/YellowDwarf.jpg"
    F -> attribute "src" "file:///C:/Users/Forre/elm/soi-gen/YellowWhiteDwarf.jpg"
    B -> attribute "src" "file:///C:/Users/Forre/elm/soi-gen/BlueGiant.gif"
    ) :: (style "width" "100%") :: (style "height" "80%") :: []

starStyle: List (Html.Attribute msg)
starStyle = 
    [ style "padding-top" "0.9%"
    , style "padding-left" "0.9%"
    , style "padding-right" "0.9%"
    , style "font-weight" "bold"
    ]

planetContainerStyle: List (Html.Attribute msg)
planetContainerStyle = 
    [ style "display" "flex"
    , style "height" "100px"
    , style "width" "100%"
    ]

innerView: List PlanetaryFeature -> List (Html Msg)
innerView = map (\p -> div (planetStyle "#EEB0B0") [ planetView p ])

habitableView: List PlanetaryFeature -> List (Html Msg)
habitableView = map (\p -> div (planetStyle "#B0EEB0") [ planetView p ])

outerView: List PlanetaryFeature -> List (Html Msg)
outerView = map (\p -> div (planetStyle "LightSteelBlue") [ planetView p ])
--i for names
planetStyle: String -> List (Html.Attribute msg)
planetStyle color = 
    [ style "height" "100px"
    , style "width" "200px"
    , style "margin" "auto"
    , style "background-color" color
    , style "padding-top" "0.9%"
    , style "padding-left" "0.9%"
    , style "font-weight" "bold"
    , style "font-size" "medium"
    ]

planetView: PlanetaryFeature -> Html Msg
planetView planet = case planet of
    DerelictStation o -> div [] [ text (showPlanet planet), p planetDetailStyle [ text (showDerelictStationOrigin o) ] ]
    StarshipGraveyard o -> div [] [ text (showPlanet planet), p planetDetailStyle [ text (showGraveyardOrigin o) ] ]
    x -> text (showPlanet x)



planetDetailStyle: List (Html.Attribute msg)
planetDetailStyle = 
    [ style "font-weight" "normal"
    , style "font-style" "italic"
    , style "font-size" "small"
    ]
--

generateButton = div []
    [ button ((onClick Generate) :: buttonStyle)
             [ text "Generate" ]
    ]

buttonStyle: List (Html.Attribute msg)
buttonStyle = 
    [ style "width" "300px"
    , style "margin-top" "10px"
    , style "font-size" "18px"
    , style "border" "none"
    , style "background-color" "#397cd5"
    , style "color" "white"
    ]
