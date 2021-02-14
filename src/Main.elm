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
    BlueGiant -> { inner = 3, habitable = 3, outer = 3 }

showStar: Star -> String
showStar star = case star of 
    RedDwarf -> "Red Dwarf"
    YellowDwarf -> "Yellow Dwarf"
    BlueGiant -> "Blue Giant"

--RockyPlanet (List (Maybe OrbitalFeature)) | GasGiant (List (Maybe PlanetaryFeature)) | AsteroidBelt | AsteroidCluster
type PlanetaryFeature = RockyPlanet | GasGiant | AsteroidBelt | AsteroidCluster

showPlanet: PlanetaryFeature -> String
showPlanet p = case p of
    RockyPlanet -> "Planet"
    GasGiant -> "Gas Giant"
    AsteroidBelt -> "Asteroid Belt"
    AsteroidCluster -> "Asteroid Cluster"

type OrbitalFeature = Moon

init: () -> (Model, Cmd Msg)
init _ = ({ star = RedDwarf, innerZone = [], habitableZone = [], outerZone = [] }, Cmd.none)

--

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
                                  (Random.list (regionSizes star).inner randomPlanet)
                                  (Random.list (regionSizes star).habitable randomPlanet)
                                  (Random.list (regionSizes star).outer randomPlanet)


randomPlanet: Random.Generator PlanetaryFeature
randomPlanet = Random.uniform RockyPlanet [ GasGiant, AsteroidBelt, AsteroidCluster ]


--

view: Model -> Html Msg
view model = div []
    [ system model
    , generateButton
    ]

system: Model -> Html Msg
system {star, innerZone, habitableZone, outerZone} = div []
    [ text (showStar star)
    , div zoneStyle (map (\p -> div [] [ text (showPlanet p) ]) innerZone)
    , div zoneStyle (map (\p -> div [] [ text (showPlanet p) ]) habitableZone)
    , div zoneStyle (map (\p -> div [] [ text (showPlanet p) ]) outerZone)
    ]

zoneStyle: List (Html.Attribute msg)
zoneStyle =
    [ style "border" "5px solid black" ]

--

generateButton = div []
    [ button ([ onClick Generate ] ++ buttonStyle)
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
