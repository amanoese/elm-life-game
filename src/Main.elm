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
  repeat 100 (repeat 100 1)

flattenCells: List (List Int) -> List(Int,Int,Int)
flattenCells c =
  concat (indexedMap (\y list -> indexedMap (\x v-> (y,x,v)) list) c)

type alias Cell = { y:Int , x:Int, v:Int }
cells:List Cell
cells =
  cellsNum
  |> flattenCells
  |> List.map (\(y,x,v)->Cell y x v)

cellToSvgRect: List Cell -> List (S.Svg msg)
cellToSvgRect =
  List.map (\cell -> S.rect 
    [ S.x <| String.fromInt <| cell.x * 5
    , S.y <| String.fromInt <| cell.y * 5
    , S.width "5"
    , S.height "5" ] 
    [] )

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
