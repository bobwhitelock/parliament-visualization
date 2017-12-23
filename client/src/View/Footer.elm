module View.Footer exposing (footer)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tachyons exposing (classes)
import Tachyons.Classes as TC exposing (..)


footer : Html msg
footer =
    Html.footer [ classes [ pv2, ph3, ph5_m, ph6_l ] ]
        [ div [ classes [ tc, mt3 ] ]
            (List.intersperse
                (text "·")
                [ footerLink "Built by Bob Whitelock" "https://github.com/bobwhitelock"
                , footerLink "Source on GitHub" "https://github.com/bobwhitelock/parliament-visualization"
                , footerLink "Data from TheyWorkForYou" "https://www.theyworkforyou.com"
                ]
            )
        ]


footerLink : String -> String -> Html msg
footerLink name url =
    a
        [ classes [ f6, dib, ph2, link, mid_gray, dim ]
        , href url
        , title name
        ]
        [ text name ]
