module ContactUs exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)
import Http exposing (..)
import Html exposing (text, img, h1, p, div, span)
import Html.Attributes exposing (style, classList, src)
import GraphQl exposing (Operation, Variables, Query, Named)
import Debug exposing (log)
import Tachyons exposing (..)
import Tachyons.Classes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


graphqlEndpoint : String
graphqlEndpoint =
    -- "http://138.68.187.161:8000/graphql"
    "http://localhost:8000/graphql"


pageTitle : String
pageTitle =
    "contact-us"


type Msg
    = GotContent (Result Error PageBy)


type alias Avatar =
    { url : Maybe String
    }


type alias Author =
    { name : Maybe String
    , avatar : Maybe Avatar
    }


type alias PageContent =
    { title : Maybe String
    , content : Maybe String
    , author : Maybe Author
    }


type alias PageBy =
    { pageBy : PageContent
    }


type alias Model =
    { title : String
    , content : String
    , author : String
    , avatar : String
    }


initModel : Model
initModel =
    Model "" "" "" ""


decodeAvatar : Decoder Avatar
decodeAvatar =
    Decode.map Avatar
        (maybe (field "url" string))


decodeAuthor : Decoder Author
decodeAuthor =
    Decode.map2 Author
        (maybe (field "name" string))
        (maybe (field "avatar" decodeAvatar))


decodePageBy : Decoder PageBy
decodePageBy =
    Decode.map PageBy
        (field "pageBy" decodePageContent)


decodePageContent : Decoder PageContent
decodePageContent =
    Decode.map3 PageContent
        (maybe (field "title" string))
        (maybe (field "content" string))
        (maybe (field "author" decodeAuthor))


pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "query"
        [ GraphQl.field "pageBy"
            |> GraphQl.withArgument "uri" (GraphQl.string pageTitle)
            |> GraphQl.withSelectors
                [ GraphQl.field "title"
                , GraphQl.field "content"
                , GraphQl.field "author"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "name"
                        , GraphQl.field "avatar"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "url"
                                ]
                        ]
                ]
        ]
        |> GraphQl.withVariables []


baseRequest :
    Operation Query Variables
    -> Decoder PageBy
    -> GraphQl.Request Query Variables PageBy
baseRequest =
    GraphQl.query graphqlEndpoint


sendRequest : Cmd Msg
sendRequest =
    baseRequest pageRequest decodePageBy
        |> GraphQl.send GotContent


extractAuthor : Maybe Author -> String
extractAuthor author =
    case author of
        Just val ->
            Maybe.withDefault "" val.name

        Nothing ->
            "unknown author"


extractAvatar : Maybe Author -> String
extractAvatar author =
    case author of
        Just val ->
            extractUrl val.avatar

        Nothing ->
            initModel.avatar


extractUrl : Maybe Avatar -> String
extractUrl avatar =
    case avatar of
        Just val ->
            Maybe.withDefault initModel.avatar val.url

        Nothing ->
            initModel.avatar


responseToModel : PageBy -> Model -> Model
responseToModel { pageBy } model =
    { model
        | title = Maybe.withDefault model.title pageBy.title
        , content = Maybe.withDefault model.content pageBy.content
        , author = extractAuthor pageBy.author
        , avatar = extractAvatar pageBy.author
    }


renderHtml : String -> Html.Attribute msg
renderHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


init : ( Model, Cmd Msg )
init =
    ( initModel, sendRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok response) ->
            ( responseToModel response model, Cmd.none )

        GotContent (Err err) ->
            ( { model | content = toString err }, Cmd.none )


view : Model -> Html.Html Msg
view { title, content, author, avatar } =
    div [ classes [ pa3, sans_serif ], style [ ( "maxWidth", "32rem" ), ( "margin", "auto" ) ] ]
        [ Html.h1 [] [ text title ]
        , p [ renderHtml content ] []
        ]
