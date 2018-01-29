module Elements exposing (navbar, footer)

import Html exposing (nav, a, text, div)
import Html.Attributes exposing (href)
import Config exposing (baseUrl)
import Tachyons exposing (..)
import Tachyons.Classes
    exposing
        ( flex
        , items_center
        , justify_end
        , justify_center
        , bg_dark_gray
        , bg_white
        , pa2
        , sans_serif
        , mr2
        , flex_none
        , white
        , absolute
        , bottom_0
        , left_0
        , right_0
        , pv1
        , dark_gray
        , mt6
        )


navLink : String -> String -> Html.Html msg
navLink url label =
    a [ href (baseUrl ++ url), classes [ mr2, flex_none, white ] ] [ text label ]


navbar : Html.Html msg
navbar =
    nav [ classes [ flex, items_center, justify_end, bg_dark_gray, pa2, sans_serif ] ]
        [ navLink "/" "Home"
        , navLink "/articles" "Articles"
        , navLink "/about-us" "About Us"
        ]


footer : Bool -> Html.Html msg
footer loading =
    let
        baseClasses =
            [ flex
            , items_center
            , justify_center
            , pv1
            , dark_gray
            , bg_white
            , mt6
            ]

        loadingClasses =
            if loading then
                List.append baseClasses [ absolute, right_0, left_0, bottom_0 ]
            else
                baseClasses
    in
        div [ classes loadingClasses ]
            [ text "Christian Muslim Forum © copyright 2018"
            ]
