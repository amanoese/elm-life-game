module Main exposing (..)

import Random
import List exposing (..)
import Debug exposing (log)

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

cellToSvgRect: List Cell -> List (Svg msg)
cellToSvgRect =
  List.map (\cell -> rect
    [ S.x <| String.fromInt <| cell.x * cellSize
    , S.y <| String.fromInt <| cell.y * cellSize
    , S.width <| String.fromInt <| cellSize
    , S.height <| String.fromInt <| cellSize
    , S.style <| "fill:rgb(" ++ (String.join "," <| repeat 3 (String.fromInt <| cell.v * 255)) ++ ")"
    ]
    [] )

boxPattern:List (Int,Int)
boxPattern=
  let pattern = range -1 1
  in
      List.map (\n -> List.map (\m -> (n,m)) pattern) pattern
  |>  concat

--updateWorld: List Cell -> List Cell
--updateWorld=

split : Int -> List a -> List (List a)
split i list =
  case take i list of
    [] -> []
    head -> head :: split i (drop i list)

type alias Model = { cells:List Cell }

type Msg
  =  Start
  | FlatList (List Int)

init : () -> (Model, Cmd Msg)
init flags =
  (Model <| initCells 0,Cmd.none)

update: Msg -> Model-> (Model , Cmd Msg)
update msg model =
  let _ = Debug.log "yo"
  in
  case msg of
    Start ->
      (model, Random.generate FlatList (Random.list (30 * 30) (Random.int 0 1)))
    FlatList randomInts ->
      ( { model | cells = cellsToCell <| flattenCells <| split 30 randomInts } , Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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
        [ Grid.col []
            [ Button.button [ Button.primary, Button.attrs [ onClick Start ] ] [ text "start" ] ]
        ]
    ]
