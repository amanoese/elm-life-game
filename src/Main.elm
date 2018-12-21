module Main exposing (..)

import Random
import List exposing (..)
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
  concat << indexedMap (\y -> indexedMap (\x v-> (y,x,v)))

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
    _ -> "fill:rgb(255,255,255)"

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

cellState: Cell -> List Cell -> Int
cellState {x,y,v} cells =
  let mnCells
        = mooreNeighborhood (x,y)
      aliveCount
        = cells
        |> List.filter (\cell-> List.any ((==) (cell.x,cell.y)) mnCells)
        |> List.map (\cell->cell.v)
        |> foldl (+)  0
  in
    case (aliveCount,v) of
      (3,_) -> 1
      (2,1) -> 1
      (_,_) -> 0

updateWorld: List Cell -> List Cell
updateWorld cells=
  List.map (\cell->{cell | v = cellState cell cells }) cells

split : Int -> List a -> List (List a)
split i list =
  case take i list of
    [] -> []
    head -> head :: split i (drop i list)

type alias Model = { cells:List Cell, run:Bool}

type Msg
  =  Start
  | FlatList (List Int)
  | Next
  | Stop

init : () -> (Model, Cmd Msg)
init flags =
  (Model (initCells 0) False,Cmd.none)

update: Msg -> Model-> (Model , Cmd Msg)
update msg model =
  case msg of
    Start ->
      (model, Random.generate FlatList (Random.list (30 * 30) (Random.int 0 1)))
    FlatList randomInts ->
      ( { model
          | cells = cellsToCell <| flattenCells <| split 30 randomInts
          , run = True
        }
        , Cmd.none
      )
    Next ->
      ( { model | cells = updateWorld model.cells}, Cmd.none)
    Stop ->
      ( { model | run = False}, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.run of
    True -> T.every 500 (always Next)
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
        [ Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Start ] ] [ text "start" ] ]
        , Grid.col [] [ Button.button [ Button.success, Button.attrs [ onClick Next ] ] [ text "Next" ] ]
        , Grid.col [] [ Button.button [ Button.primary, Button.attrs [ onClick Stop ] ] [ text "Stop" ] ]
        ]
    ]
