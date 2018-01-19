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
    = GotContent (Result Error PageBy)


type alias PageContent =
    { title : Maybe String
    , content : Maybe String
    }


type alias PageBy =
    { pageBy : PageContent
    }


type alias Model =
    { title : String
    , content : String
    }


decodePageBy : Decoder PageBy
decodePageBy =
    Decode.map PageBy
        (field "pageBy" decodePageContent)


decodePageContent : Decoder PageContent
decodePageContent =
    Decode.map2 PageContent
        (maybe (field "title" string))
        (maybe (field "content" string))


pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "query"
        [ GraphQl.field "pageBy"
            |> GraphQl.withArgument "uri" (GraphQl.string "contact-us")
            |> GraphQl.withSelectors
                [ GraphQl.field "title"
                , GraphQl.field "content"
                ]
        ]
        |> GraphQl.withVariables []


baseRequest :
    Operation Query Variables
    -> Decoder PageBy
    -> GraphQl.Request Query Variables PageBy
baseRequest =
    GraphQl.query "http://localhost:8000/graphql"


sendRequest : Cmd Msg
sendRequest =
    baseRequest pageRequest decodePageBy
        |> GraphQl.send GotContent


responseToModel : PageBy -> Model -> Model
responseToModel { pageBy } model =
    { model
        | title = Maybe.withDefault model.title pageBy.title
        , content = Maybe.withDefault model.content pageBy.content
    }


setHtml : String -> Html.Attribute msg
setHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


init : ( Model, Cmd Msg )
init =
    ( Model "..." "...", sendRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok response) ->
            ( responseToModel response model, Cmd.none )

        GotContent (Err err) ->
            ( { model | content = toString err }, Cmd.none )


view : Model -> Html.Html Msg
view { title, content } =
    div []
        [ h1 [ setHtml title ] []
        , p [ setHtml content ] []
        ]
