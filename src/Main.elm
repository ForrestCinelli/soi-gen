--todo:
    --generates when you press g
    --use probability distributions to handle the 01-20 case where a feature is "None"
    --orbital features
        --type 
        --generator
        --view
    --names
    --make planets and features match book rprecisely
    --box which shows the random seed, and if you enter a custom one it uses that one. 
    --pic can get squished horizontally instead of sizing down
    --g and enter don't work on mac. 
    --not suck on moble

module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text, p)
import Html.Events exposing (onClick, on, keyCode)
import Html.Attributes exposing (style, attribute)
import List exposing (head, length, tail, map, concatMap)
import Json.Decode as D
import Maybe
import Random
import Random.Extra
import Tuple exposing (first, second)

import BasicTypes exposing (Zone(..))
import PlanetaryFeature exposing
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
    , showAtmosphere
    , showTemperature
    , showAtmosphereComposition
    , showGasBody
    , showGasGravity
    , showGraveyardOrigin
    , showDerelictStationOrigin
    )

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
systemFeatures = [Bountiful, GravityTides, Haven, IllOmened, PirateDen, RuinedEmpire, Starfarers, StellarAnomaly, WarpStasis, WarpTurbulence]

planetsForStar feature star = Random.map3 (\i -> \h -> \o -> Model star feature i h o)
                                          (Random.list (regionSizes star).inner randomInner)
                                          (Random.list (regionSizes star).habitable randomHabitable)
                                          (Random.list (regionSizes star).outer randomOuter)

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

init: () -> (Model, Cmd Msg)
init _ = ({ star = M, feature = IllOmened, innerZone = [], habitableZone = [], outerZone = [] }, Random.generate NewSystem randomModel)

----

type Msg = Generate | NewSystem Model | Keydown Int

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
    NewSystem s -> (s, Cmd.none)
    Generate ->
        ( model
        , Random.generate NewSystem randomModel
        )
    Keydown key -> if (key == 13 || key == 71) then (update Generate model) else (model, Cmd.none)

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

----

view: Model -> Html Msg
view model = div [ onKeydown ]
    [ system model
    ]

onKeydown: Html.Attribute Msg
onKeydown = on "keydown" (D.map Keydown keyCode) 

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
    , style "grid-template-rows" "fit-content(0%) 1fr"
    , style "width" "100%"
    , style "margin" "0"
    ]

innerView: List PlanetaryFeature -> List (Html Msg)
innerView = concatMap (
        \p -> 
            let pv = planetView Inner p 
            in [ first pv, second pv ]
    )

habitableView: List PlanetaryFeature -> List (Html Msg)
habitableView = concatMap (
        \p -> 
            let pv = planetView Habitable p 
            in [ first pv, second pv ]
    )

outerView: List PlanetaryFeature -> List (Html Msg)
outerView = concatMap (
        \p -> 
            let pv = planetView Outer p 
            in [ first pv, second pv ]
    )
--<i> for names

planetView: Zone -> PlanetaryFeature -> (Html Msg, Html Msg)
planetView zone planet = case planet of
    DerelictStation o ->
        ( p (headerStyle zone) [ text (showPlanet planet) ]
        , p ((planetDetailStyle zone) ++ detailStyle) [ text (showDerelictStationOrigin o) ]
        )
    StarshipGraveyard o -> 
        ( p (headerStyle zone) [ text (showPlanet planet) ]
        , p ((planetDetailStyle zone) ++ detailStyle) [ text (showGraveyardOrigin o) ]
        )
    RockyPlanet (TerrestrialPlanet body gravity atmosphere temperature orbitalFeatures) -> 
        ( p (headerStyle zone) [ text (showPlanet planet) ]
        , div ((planetDetailStyle zone))
            [ p detailStyle [ text ((showBody body) ++ " with " ++ (showGravity gravity)) ]
            , p detailStyle [ text ((showTemperature temperature) ++ " world") ]
            , p detailStyle [ text (showAtmosphere atmosphere) ]
            ]
        )
    GasGiant (GiantPlanet body gravity orbitalFeatures) ->
        ( p (headerStyle zone) [ text (showPlanet planet) ]
        , p ((planetDetailStyle zone) ++ detailStyle) [ text ((showGasBody body) ++ " with " ++ (showGasGravity gravity)) ]
        )
    x -> 
        ( p (headerStyle zone) [ text (showPlanet x) ]
        , div ((planetDetailStyle zone) ++ detailStyle) []
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
    ]

planetDetailStyle: Zone -> List (Html.Attribute msg)
planetDetailStyle z = 
    [ style "background-color" (case z of
        Inner -> "#EEB0B0"
        Habitable -> "#B0EEB0"
        Outer -> "LightSteelBlue"
        )
    , row 2
    ]

headerStyle: Zone -> List(Html.Attribute msg)
headerStyle z = 
    [ style "background-color" (case z of
        Inner -> "#DE8686"
        Habitable -> "#86DE86"
        Outer -> "#819DC0"
        )
    , style "font-weight" "bold"
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
