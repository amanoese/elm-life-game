module Main exposing (..)

import Browser
import List exposing (..)
import Html exposing (..) --(h1, div, p, text)
import Html.Attributes exposing (..) -- (class..)
import Svg as S
import Svg.Attributes as S
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col

cellsNum: List (List Int)
cellsNum =
  repeat 10 (repeat 10 1)

flattenCellsNum: List (List Int) -> List (Int,Int)
flattenCellsNum =
  concatMap (indexedMap Tuple.pair)

flattenCellsNumIndex: List (Int,Int) -> List(Int,Int,Int)
flattenCellsNumIndex =
  indexedMap (\i (a,b) -> (i,a,b))

type alias Cell = { x:Int , y:Int, v:Int }
cells:List Cell
cells =
  cellsNum
  |> flattenCellsNum
  |> flattenCellsNumIndex
  |> List.map (\(x,y,v)->Cell x y v)

cellToSvgRect: List Cell -> List (S.Svg msg)
cellToSvgRect =
  List.map (\cell -> S.rect [ S.x <| String.fromInt <| cell.x * 5, S.y <| String.fromInt <| cell.y * 5  , S.width "5" , S.height "5" ] [] )


main =
  Browser.sandbox {
      init = { cells =  cells }
    , update = update, view = view 
    }

update () model = model

view model =
  Grid.container []
    [ CDN.stylesheet
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col []
            [ div [] [ text "Hello,Elm!" ] ]
        ]
    , Grid.row [ Row.attrs [ class "text-center align-middle" ] ]
        [ Grid.col []
            [ S.svg
                [ S.width "500"
                , S.height "500"
                , S.viewBox "0 0 500 500"
                ]
                (cellToSvgRect model.cells)
            ]
        ]
    ]
