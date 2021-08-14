module Main exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Random
import Time
import Debug
import Array2D
import Bootstrap.CDN
import Bootstrap.Button


type alias Grid = Array2D.Array2D Bool
type alias Size = { width: Int, height: Int }
type alias Position = { x: Int, y: Int }
type alias Model = 
    { grid: Grid
    , state: State
    }

-- Constants

gridSize : Size
gridSize = Size 100 60 

cellSize : Size
cellSize = Size 10 10

gridColor : String
gridColor = "grey"

backgroundColor : String
backgroundColor = "white"

cellColor : String
cellColor = "black"

-- tick period in mS
tickPeriod : Float
tickPeriod = 200

type Msg
    = Tick Time.Posix
    | RandomCell Position
    | Start
    | Stop
    | GridClicked

type State
    = Active
    | Inactive

generateGrid : Size -> Grid
generateGrid size = Array2D.repeat size.width size.height False

randomPosition : Random.Generator Position 
randomPosition = 
    let
        randomPair = Random.pair (Random.int 0 (gridSize.width - 1)) (Random.int 0 (gridSize.height - 1))
    in
        Random.map (\p -> { x = Tuple.first p, y = Tuple.second p }) randomPair

-- randomGrid : Int -> Cmd Msg
-- randomGrid n = Random.generate RandomCell randomPosition


init : () -> (Model, Cmd Msg)
init _ =
    ( { grid = generateGrid gridSize
      , state = Inactive
      }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = 
    case msg of
        Tick time -> 
            let
                log = Debug.log "event:" "Tick"
                grid = model.grid
                newModel =
                    { model
                    | grid = if model.state == Active then evolve grid else grid }
            in
                ( newModel
                , Cmd.none
                )
        RandomCell pos -> 
            let
                newGrid = Array2D.set pos.x pos.y True model.grid
                newModel = 
                    { model
                    | grid = newGrid }
            in
              (newModel, Cmd.none)
        Start -> 
            let
                _ = Debug.log "event:" "Start"
                newModel =
                    { model
                    | state = Active }
            in
                (newModel, Cmd.none)
        Stop -> 
            let
                _ = Debug.log "event:" "Stop"
                newModel =
                    { model
                    | state = Active }
            in
                (newModel, Cmd.none)
        GridClicked -> 
            (model, Cmd.none)

view : Model -> Html Msg
view model = 
    Html.div []
        [ Bootstrap.CDN.stylesheet
        , Html.div [] [ renderGrid model ]
        , Html.div [ Html.Attributes.style "margin-top" "10px" ]
            [ Bootstrap.Button.button
                [ Bootstrap.Button.onClick Start
                , Bootstrap.Button.primary ] [ text "Start" ]
            , Html.text " "
            , Bootstrap.Button.button
                [ Bootstrap.Button.onClick Stop
                , Bootstrap.Button.secondary ] [ text "Stop" ]
            ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Time.every tickPeriod Tick

renderGrid : Model -> Html Msg
renderGrid model = 
    let
        canvasWidth = gridSize.width * cellSize.width
        canvasHeight = gridSize.height * cellSize.height
    in
        svg
            [ width (String.fromInt canvasWidth)
            , height (String.fromInt canvasHeight)
            , viewBox (String.join " " (List.map String.fromInt [0, 0, canvasWidth, canvasHeight]))
            , Svg.Events.onClick GridClicked
            ]
            (
                rect
                [ width (String.fromInt canvasWidth)
                , height (String.fromInt canvasHeight)
                , stroke gridColor
                , fill backgroundColor
                ] []
                :: (List.map (
                    \x -> line
                    [ x1 (String.fromInt (x * cellSize.width))
                    , x2 (String.fromInt (x * cellSize.width))
                    , y1 "0"
                    , y2 (String.fromInt canvasHeight)
                    , stroke gridColor
                    ] [])
                    (List.range 1 (gridSize.width - 1)))
                ++ (List.map (
                    \x -> line
                    [ y1 (String.fromInt (x * cellSize.height))
                    , y2 (String.fromInt (x * cellSize.height))
                    , x1 "0"
                    , x2 (String.fromInt canvasWidth)
                    , stroke gridColor
                    ] [])
                    (List.range 1 (gridSize.height - 1)))
            )

renderCell : Position -> Svg Msg
renderCell pos =
  rect [ width (String.fromInt cellSize.width)
       , height (String.fromInt cellSize.height)
       , x (String.fromInt (pos.x * cellSize.width))
       , y (String.fromInt (pos.y * cellSize.height))
       , fill cellColor
       ] []

evolve : Grid -> Grid
evolve grid =
    grid

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }