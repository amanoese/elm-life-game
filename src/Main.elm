module Main exposing (..)

import Browser
import Html exposing (..) --(h1, div, p, text)
import Html.Attributes exposing (..) -- (class..)
import Svg as S
import Svg.Attributes as S
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col

main =
  Browser.sandbox { init = Cmd.none, update = update, view = view }

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
                [ S.width "300"
                , S.height "300"
                , S.viewBox "0 0 300 300"
                ]
                [ S.rect
                  [ S.x "50"
                  , S.y "50"
                  , S.width "50"
                  , S.height "50"
                  ]
                  []
                ]
            ]
        ]
    ]
