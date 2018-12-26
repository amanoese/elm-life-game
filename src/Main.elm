module Main exposing (..)

import Random
import List exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import Debug exposing (log)
import Time as T

import Browser
import Html exposing (..) --(h1, div, p, text)
import Html.Attributes exposing (..) -- (class..)
import Html.Events exposing (onClick)
import Svg exposing (Svg,svg,rect)
import Svg.Attributes as S
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

type alias CellSize = Int
type alias Cell = { x:Int , y:Int, v:Int }

flattenCells: List (List Int) -> List Cell
flattenCells=
  concat << indexedMap (\y -> indexedMap (\x v-> Cell x y v))

initCells: Int -> List Cell
initCells=
  flattenCells << repeat 30 << repeat 30

styleColor v =
  case v of
    1 -> "fill:rgb(0,0,0)"
    _ -> "fill:rgb(235,235,235)"

cellToSvgRect:CellSize -> List Cell -> List (Svg msg)
cellToSvgRect cellSize =
  List.map (\cell -> rect
    [ S.x <| String.fromInt <| cell.x * cellSize
    , S.y <| String.fromInt <| cell.y * cellSize
    , S.width <| String.fromInt <| cellSize
    , S.height <| String.fromInt <| cellSize
    , S.style <| styleColor cell.v
    ]
    [] )

mooreNeighborhood: (Int, Int) -> List (Int, Int)
mooreNeighborhood (x,y) =
  let pattern = range -1 1
  in
  pattern
  |> List.map (\n -> List.map (\m -> (n,m)) pattern)
  |> concat
  |> List.filter ((/=) (0,0))
  |> List.map (Tuple.mapBoth ((+) x) ((+) y))

cellState: Cell -> Dict (Int,Int) Int -> Int
cellState {x,y,v} dictDictCells =
  let aliveCount
        = mooreNeighborhood (x,y)
        |> List.map (\key->
            Dict.get key dictDictCells
            |> withDefault 0)
        |> List.foldl (+)  0
  in
    case (aliveCount,v) of
      (3,_) -> 1
      (2,1) -> 1
      _ -> 0

updateState: List Cell -> List Cell
updateState cells =
  let dictDictCells
        = cells
        |> List.map (\{x,y,v}-> ((x, y), v))
        |> Dict.fromList
  in
      List.map (\cell->{cell | v = cellState cell dictDictCells}) cells

split : Int -> List a -> List (List a)
split i list =
  case take i list of
    [] -> []
    head -> head :: split i (drop i list)

type alias Model =
  { cells:List Cell
  , run:Bool
  , generation:Int
  , cellSize:Int
  , numberOfCells:Int
  }


type Msg
  =  Init
  | RandomList (List Int)
  | Start
  | Next
  | Stop

init : () -> (Model, Cmd Msg)
init flags =
  ( { cells = initCells 0, run = False, generation = 0 , cellSize = 20 , numberOfCells = 30 } ,Cmd.none)

update: Msg -> Model-> (Model , Cmd Msg)
update msg model =
  let { cells, numberOfCells } = model
  in
  case msg of
    Init ->
      (model, Random.generate RandomList (Random.list (numberOfCells ^ 2) (Random.int 0 1)))
    RandomList randomInts ->
      ( { model | cells = flattenCells <| split numberOfCells randomInts }
        , Cmd.none
      )
    Start ->
      ( { model | run = True }, Cmd.none)
    Next ->
      ( { model | cells = updateState cells }, Cmd.none)
    Stop ->
      ( { model | run = False }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.run of
    True -> T.every 200 (always Next)
    _ ->  Sub.none

main =
  Browser.element  { init = init , update = update, view = view ,subscriptions = subscriptions }

dummy = Debug.log "Debug!"

view model =
  let boxWidth =   String.fromInt <| model.cellSize * model.numberOfCells
      boxHeight = String.fromInt <| model.cellSize * model.numberOfCells
  in
  Grid.container []
    [ CDN.stylesheet
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col [] [ div [] [ text "Hello,Elm!" ] ]
        ]
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col []
            [ svg
                [ S.width boxWidth
                , S.height boxHeight
                , S.viewBox <| String.join " " ["0","0",boxWidth,boxHeight]
                ]
                (cellToSvgRect model.cellSize model.cells)
            ]
        ]
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Init ] ] [ text "Init" ] ]
        , Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Start ] ] [ text "Start" ] ]
        , Grid.col [] [ Button.button [ Button.success, Button.attrs [ onClick Next ] ] [ text "Next" ] ]
        , Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Stop ] ] [ text "Stop" ] ]
        ]
    ]
