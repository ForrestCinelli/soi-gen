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
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import List exposing (head, length, tail, map)
import Maybe
import Random

main = Browser.element { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }

type alias Model = { star: Star, innerZone: List PlanetaryFeature, habitableZone: List PlanetaryFeature, outerZone: List PlanetaryFeature }

----

type Star = RedDwarf | YellowDwarf | BlueGiant
stars = [RedDwarf, YellowDwarf, BlueGiant]

regionSizes: Star -> { inner: Int, habitable: Int, outer: Int }
regionSizes star = case star of 
    RedDwarf -> { inner = 3, habitable = 3, outer = 5 }
    YellowDwarf -> { inner = 3, habitable = 5, outer = 3 }
    BlueGiant -> { inner = 5, habitable = 3, outer = 3 }

showStar: Star -> String
showStar star = case star of 
    RedDwarf -> "Red Dwarf"
    YellowDwarf -> "Yellow Dwarf"
    BlueGiant -> "Blue Giant"

--RockyPlanet (List (Maybe OrbitalFeature)) | GasGiant (List (Maybe PlanetaryFeature)) | AsteroidBelt | AsteroidCluster
type PlanetaryFeature = RockyPlanet | GasGiant | AsteroidBelt | AsteroidCluster
type TerrestrialPlanet = TerrestrialPlanet PlanetBody PlanetGravity Atmosphere Temperature (List OrbitalFeature)
type GiantPlanet = GiantPlanet GasBody GasGravity (List OrbitalFeature)

type GasBody = Dwarf | Giant | Massive -- massive > giant
type GasGravity = Weak | String | Powerful | Titanic

type PlanetBody = LowMass | Small | SmallDense | Large | LargeDense | Vast
type PlanetGravity = Low | Normal | High

type Temperature = Burning | Hot | Temperate | Cold | Ice

type Atmosphere = None | Thin AtmosphereComposition | Moderate AtmosphereComposition | Heavy AtmosphereComposition
type AtmosphereComposition = Deadly | Corrosive | Toxic | Tainted | Pure

tmpPlanet = TerrestrialPlanet LowMass Low None Burning []
tmpGiant = GiantPlanet Dwarf Weak []

showPlanet: PlanetaryFeature -> String
showPlanet p = case p of
    RockyPlanet -> "Planet"
    GasGiant -> "Gas Giant"
    AsteroidBelt -> "Asteroid Belt"
    AsteroidCluster -> "Asteroid Cluster"

type OrbitalFeature = Moon

init: () -> (Model, Cmd Msg)
init _ = ({ star = RedDwarf, innerZone = [], habitableZone = [], outerZone = [] }, Random.generate NewSystem randomModel)

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
randomStar = Random.uniform RedDwarf [ YellowDwarf, BlueGiant ]

planetsForStar star = Random.map3 (\i -> \h -> \o -> Model star i h o)
                                  (Random.list (regionSizes star).inner randomInner)
                                  (Random.list (regionSizes star).habitable randomHabitable)
                                  (Random.list (regionSizes star).outer randomOuter)


randomInner: Random.Generator PlanetaryFeature
randomInner = Random.uniform RockyPlanet [ GasGiant, AsteroidBelt, AsteroidCluster ]

--in habitable zone, not necessarily habitable
randomHabitable: Random.Generator PlanetaryFeature
randomHabitable = Random.uniform RockyPlanet [ GasGiant, AsteroidBelt, AsteroidCluster ]

randomOuter: Random.Generator PlanetaryFeature
randomOuter = Random.uniform RockyPlanet [ GasGiant, AsteroidBelt, AsteroidCluster ]

--randomRocky: Random.Generator TerrestrialPlanet
--randomRocky = undefined

--randomGas: Random.Generator GiantPlanet
--randomGas = undefined


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
starView star = div starStyle [ (text (showStar star)) ]

starStyle: List (Html.Attribute msg)
starStyle = 
    [ style "padding-top" "0.9%"
    , style "padding-left" "0.9%"
    , style "font-weight" "bold"
    ]

planetContainerStyle: List (Html.Attribute msg)
planetContainerStyle = 
    [ style "display" "flex"
    , style "height" "100px"
    , style "width" "100%"
    ]

innerView: List PlanetaryFeature -> List (Html Msg)
innerView = map (\p -> div (planetStyle "#EEB0B0") [ text (showPlanet p) ])

habitableView: List PlanetaryFeature -> List (Html Msg)
habitableView = map (\p -> div (planetStyle "#B0EEB0") [ text (showPlanet p) ])

outerView: List PlanetaryFeature -> List (Html Msg)
outerView = map (\p -> div (planetStyle "LightSteelBlue") [ text (showPlanet p) ])
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
    , style "font-size" "16px"
    , style "border" "none"
    , style "background-color" "#397cd5"
    , style "color" "white"
    ]