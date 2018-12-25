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

cellsNum: Int -> List (List Int)
cellsNum =
  repeat 30 << repeat 30

cellSize:Int
cellSize= 20

flattenCells: List (List Int) -> List( Int, Int, Int )
flattenCells=
  concat << indexedMap (\y -> indexedMap (\x v-> (x,y,v)))

type alias Cell = { y:Int , x:Int, v:Int }
cellsToCell:List(Int,Int,Int) -> List Cell
cellsToCell=
  List.map (\(y,x,v)-> Cell y x v)

initCells: Int -> List Cell
initCells=
  cellsToCell << flattenCells << cellsNum

styleColor v =
  case v of
    1 -> "fill:rgb(0,0,0)"
    _ -> "fill:rgb(235,235,235)"

cellToSvgRect: List Cell -> List (Svg msg)
cellToSvgRect =
  List.map (\cell -> rect
    [ S.x <| String.fromInt <| cell.x * cellSize
    , S.y <| String.fromInt <| cell.y * cellSize
    , S.width <| String.fromInt <| cellSize
    , S.height <| String.fromInt <| cellSize
    , S.style <| styleColor cell.v
    ]
    [] )

boxPattern:List (Int,Int)
boxPattern=
  let pattern = range -1 1
  in
      List.map (\n -> List.map (\m -> (n,m)) pattern) pattern
  |>  concat
  |>  List.filter ((/=) (0,0))

mooreNeighborhood: (Int, Int) -> List (Int, Int)
mooreNeighborhood (x,y) =
  List.map (\(x2,y2) -> (x + x2, y + y2)) boxPattern

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

updateWorld: List Cell -> List Cell
updateWorld cells =
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

type alias Model = { cells:List Cell, run:Bool, generation:Int}

type Msg
  =  Init
  | RandomList (List Int)
  | Start
  | Next
  | Stop

init : () -> (Model, Cmd Msg)
init flags =
  (Model (initCells 0) False 0,Cmd.none)

update: Msg -> Model-> (Model , Cmd Msg)
update msg model =
  case msg of
    Init ->
      (model, Random.generate RandomList (Random.list (30 * 30) (Random.int 0 1)))
    RandomList randomInts ->
      ( { model | cells = cellsToCell <| flattenCells <| split 30 randomInts }
        , Cmd.none
      )
    Start ->
      ( { model | run = True }, Cmd.none)
    Next ->
      ( { model | cells = updateWorld model.cells }, Cmd.none)
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
  Grid.container []
    [ CDN.stylesheet
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col [] [ div [] [ text "Hello,Elm!" ] ]
        ]
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col []
            [ svg
                [ S.width "600"
                , S.height "600"
                , S.viewBox "0 0 600 600"
                ]
                (cellToSvgRect model.cells)
            ]
        ]
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Init ] ] [ text "Init" ] ]
        , Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Start ] ] [ text "Start" ] ]
        , Grid.col [] [ Button.button [ Button.success, Button.attrs [ onClick Next ] ] [ text "Next" ] ]
        , Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Stop ] ] [ text "Stop" ] ]
        ]
    ]
