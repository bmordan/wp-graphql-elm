module App exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import GraphQl exposing (Operation, Variables, Query, Named)
import Debug exposing (log)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type Msg
    = GotContent (Result Error PageContent)


type alias PageContent =
    { title : Maybe String
    , content : Maybe String
    }


type alias Model =
    { title : String
    , content : String
    }


decodePageContent : Decoder PageContent
decodePageContent =
    Decode.map2 PageContent
        (maybe (field "title" string))
        (maybe (field "content" string))


pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "what"
        [ GraphQl.field "posts"
            |> GraphQl.withSelectors
                [ GraphQl.field "postTypeInfo"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "id"
                        ]
                ]
        ]
        |> GraphQl.withVariables []


baseRequest :
    Operation Query Variables
    -> Decoder PageContent
    -> GraphQl.Request Query Variables PageContent
baseRequest =
    GraphQl.query "http://localhost:8000/graphql"


sendRequest : String -> Cmd Msg
sendRequest page =
    baseRequest pageRequest decodePageContent
        |> GraphQl.addVariables [ ( "uri", Encode.string page ) ]
        |> GraphQl.send GotContent


responseToModel : PageContent -> Model -> Model
responseToModel content model =
    { model | title = (toString content) }


init : ( Model, Cmd Msg )
init =
    ( Model "..." "...", sendRequest "contact-us" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok response) ->
            ( { model | content = toString response }, Cmd.none )

        GotContent (Err err) ->
            ( { model | content = toString err }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ model.title |> toString |> text ]
        , p [] [ model.content |> toString |> text ]
        ]
