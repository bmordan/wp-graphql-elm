module Home exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, at, field, maybe, int, string, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional, custom, requiredAt)
import Http exposing (..)
import Html exposing (a, text, img, h1, h2, p, div, span)
import Html.Attributes exposing (style, classList, src)
import GraphQl exposing (Operation, Variables, Query, Named)
import Debug exposing (log)
import Tachyons exposing (..)
import Tachyons.Classes exposing (..)
import Config exposing (graphqlEndpoint)
import Elements exposing (navbar, footer)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


pageTitle : String
pageTitle =
    "home"


defaultImage : String
defaultImage =
    "http://localhost/wp-content/uploads/2018/01/CMF.png"


type alias Data =
    { pageBy : Page
    }


type alias Page =
    { content : Maybe String
    , featuredImage : Maybe FeaturedImage
    }


type alias FeaturedImage =
    { sourceUrl : String
    }


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "pageBy" decodePage


decodePage : Decoder Page
decodePage =
    decode Page
        |> required "content" (nullable string)
        |> required "featuredImage" (nullable decodeFeaturedImage)


decodeFeaturedImage : Decoder FeaturedImage
decodeFeaturedImage =
    decode FeaturedImage
        |> optional "sourceUrl" string defaultImage


extractPageContent : Maybe String -> String
extractPageContent content =
    case content of
        Just val ->
            val

        Nothing ->
            "no content"


extractPageFeaturedImage : Maybe FeaturedImage -> String
extractPageFeaturedImage featuredImage =
    case featuredImage of
        Just val ->
            val.sourceUrl

        Nothing ->
            defaultImage


pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "query"
        [ GraphQl.field "pageBy"
            |> GraphQl.withArgument "uri" (GraphQl.string pageTitle)
            |> GraphQl.withSelectors
                [ GraphQl.field "title"
                , GraphQl.field "content"
                , GraphQl.field "featuredImage"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "sourceUrl"
                        ]
                ]
        ]
        |> GraphQl.withVariables []


baseRequest :
    Operation Query Variables
    -> Decoder Data
    -> GraphQl.Request Query Variables Data
baseRequest =
    GraphQl.query graphqlEndpoint


sendRequest : Cmd Msg
sendRequest =
    baseRequest pageRequest decodeData
        |> GraphQl.send GotContent


extractToModel : Data -> Model -> Model
extractToModel { pageBy } model =
    { model
        | content = extractPageContent pageBy.content
        , featuredImage = extractPageFeaturedImage pageBy.featuredImage
    }


type alias Model =
    { content : String
    , featuredImage : String
    }


initModel : Model
initModel =
    Model "" ""


init : ( Model, Cmd Msg )
init =
    ( initModel, sendRequest )


type Msg
    = GotContent (Result Error Data)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok data) ->
            ( extractToModel data model, Cmd.none )

        GotContent (Err err) ->
            ( { model | content = toString err }, Cmd.none )


renderHtml : String -> Html.Attribute msg
renderHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


view : Model -> Html.Html Msg
view { featuredImage, content } =
    div []
        [ navbar
        , div [ classes [ pa3, sans_serif ], style [ ( "maxWidth", "32rem" ), ( "margin", "auto" ) ] ]
            [ div []
                [ img [ src featuredImage ] []
                ]
            , div [ renderHtml content ] []
            ]
        , footer
        ]
