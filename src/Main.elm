module Main exposing (..)

import Bootstrap.Button
import Bootstrap.CDN
import Browser
import Browser.Navigation exposing (back)
import GameOfLife.Grid as Grid
import GameOfLife.Pattern as Pattern
import Html exposing (Html)
import Html.Attributes
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Time



-- Constants


gridSize : Grid.Size
gridSize =
    Grid.Size 80 60


cellSize : Int
cellSize =
    10


gridColor : String
gridColor =
    "grey"


backgroundColor : String
backgroundColor =
    "white"


cellColor : String
cellColor =
    "black"


tickPeriod : Float
tickPeriod =
    100


type alias Model =
    { grid : Grid.Grid
    , state : State
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset
    | Clear
    | GridClicked Grid.Position


type State
    = Active
    | Inactive


init : () -> ( Model, Cmd Msg )
init _ =
    initGame ()


initGame : () -> ( Model, Cmd Msg )
initGame _ =
    ( { grid = Pattern.addPattern Pattern.gliderGun (Grid.initialize gridSize)
      , state = Inactive
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                grid =
                    model.grid

                newModel =
                    { model
                        | grid =
                            if model.state == Active then
                                Grid.evolve grid

                            else
                                grid
                    }
            in
            ( newModel
            , Cmd.none
            )

        Start ->
            let
                newModel =
                    { model
                        | state = Active
                    }
            in
            ( newModel, Cmd.none )

        Stop ->
            let
                newModel =
                    { model
                        | state = Inactive
                    }
            in
            ( newModel, Cmd.none )

        Reset ->
            initGame ()

        Clear ->
            let
                newModel =
                    { model
                        | grid = Grid.clear model.grid
                        , state = Inactive
                    }
            in
            ( newModel, Cmd.none )

        GridClicked pos ->
            let
                cell =
                    Maybe.withDefault False (Grid.get pos grid)

                grid =
                    model.grid

                newModel =
                    { model
                        | grid =
                            if model.state == Inactive then
                                Grid.set pos (not cell) grid

                            else
                                grid
                    }
            in
            ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Bootstrap.CDN.stylesheet
        , Html.h1 [] [ text "Conway's Game of Life" ]
        , Html.div [] [ renderGrid model ]
        , Html.div [ Html.Attributes.style "margin-top" "10px" ]
            [ Bootstrap.Button.button
                [ Bootstrap.Button.onClick Start
                , Bootstrap.Button.disabled (model.state == Active)
                , if model.state == Inactive then
                    Bootstrap.Button.primary

                  else
                    Bootstrap.Button.secondary
                ]
                [ text "Start" ]
            , Html.text " "
            , Bootstrap.Button.button
                [ Bootstrap.Button.onClick Stop
                , Bootstrap.Button.disabled (model.state == Inactive)
                , if model.state == Active then
                    Bootstrap.Button.primary

                  else
                    Bootstrap.Button.secondary
                ]
                [ text "Stop" ]
            , Html.text " "
            , Bootstrap.Button.button
                [ Bootstrap.Button.onClick Clear
                , Bootstrap.Button.secondary
                ]
                [ text "Clear" ]
            , Html.text " "
            , Bootstrap.Button.button
                [ Bootstrap.Button.onClick Reset
                , Bootstrap.Button.secondary
                ]
                [ text "Reset" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every tickPeriod Tick


renderGrid : Model -> Html Msg
renderGrid model =
    let
        canvasWidth =
            gridSize.width * cellSize

        canvasHeight =
            gridSize.height * cellSize

        cellsList =
            Grid.toIndexedList model.grid
    in
    svg
        [ width (String.fromInt canvasWidth)
        , height (String.fromInt canvasHeight)
        , viewBox (String.join " " <| List.map String.fromInt [ 0, 0, canvasWidth, canvasHeight ])
        ]
        ([ rect
            [ width (String.fromInt canvasWidth)
            , height (String.fromInt canvasHeight)
            , stroke gridColor
            , fill backgroundColor
            ]
            []
         ]
            ++ List.map (\x -> renderCell (Tuple.first x) (Tuple.second x)) cellsList
        )


renderCell : Grid.Position -> Bool -> Svg Msg
renderCell pos isLive =
    rect
        [ width (String.fromInt cellSize)
        , height (String.fromInt cellSize)
        , x (String.fromInt (pos.x * cellSize))
        , y (String.fromInt (pos.y * cellSize))
        , stroke gridColor
        , fill
            (if isLive then
                cellColor

             else
                backgroundColor
            )
        , Svg.Events.onClick (GridClicked (Grid.Position pos.x pos.y))
        ]
        []


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
