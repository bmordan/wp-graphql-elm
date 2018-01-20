module Posts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (..)


main =
    Navigation.program Hash
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { hash : String
    }


type Msg
    = Hash Navigation.Location


init : Location -> ( Model, Cmd Msg )
init location =
    ( Model "---", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hash location ->
            ( { model | hash = (String.dropLeft 1 location.hash) }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ classList [ ( "pa4", True ) ] ]
        [ h1 [] [ text model.hash ]
        , ul []
            [ li []
                [ a [ href "#hello" ] [ text "Hello" ]
                ]
            , li []
                [ a [ href "#world" ] [ text "World" ]
                ]
            ]
        ]
