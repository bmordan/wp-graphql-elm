module Elements exposing (..)

import Html exposing (nav, a, text, div, img)
import Html.Attributes exposing (href, src)
import Json.Encode as Encode
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


strToHtml : String -> Html.Attribute msg
strToHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


defaultImg : Html.Html msg
defaultImg =
    img [ src "http://localhost/wp-content/uploads/2018/01/placeholder.jpg" ] []
