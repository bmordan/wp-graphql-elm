module Articles exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, at, field, maybe, int, string, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional, custom, requiredAt)
import Http exposing (..)
import Html exposing (a, text, img, h1, h2, p, div, span)
import Html.Attributes exposing (style, classList, src)
import Config exposing (graphqlEndpoint, baseUrl)
import GraphQl exposing (Operation, Variables, Query, Named)
import Tachyons exposing (..)
import Tachyons.Classes exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Elements exposing (navbar, footer)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { posts : List String
    }


type alias Data =
    { posts : List }


type Msg
    = GotPosts (Result Error Data)


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts (Ok data) ->
            ( model, Cmd.none )

        GotPosts (Err err) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ navbar
        , div [] [ text "all the posts in here" ]
        , footer
        ]
