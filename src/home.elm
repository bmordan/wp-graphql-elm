module Home exposing (main)

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
    "home"


defaultImage : String
defaultImage =
    "http://localhost:8000/wp-content/uploads/2018/01/CMF-new-logo.jpg"


type alias PageBy =
    { pageBy : Page
    }


decodePageBy : Decoder PageBy
decodePageBy =
    Decode.map PageBy
        (field "pageBy" decodePage)


type alias Page =
    { content : Maybe String
    , featuredImage : Maybe FeaturedImage
    }


decodePage : Decoder Page
decodePage =
    Decode.map2 Page
        (maybe (field "content" string))
        (maybe (field "featuredImage" decodeFeaturedImage))


extractPageContent : Maybe String -> String
extractPageContent content =
    case content of
        Just val ->
            val

        Nothing ->
            "no content"


type alias FeaturedImage =
    { sourceUrl : String
    }


decodeFeaturedImage : Decoder FeaturedImage
decodeFeaturedImage =
    Decode.map FeaturedImage
        (field "sourceUrl" string)


extractPageFeaturedImage : Maybe FeaturedImage -> String
extractPageFeaturedImage featuredImage =
    case featuredImage of
        Just val ->
            val.sourceUrl

        Nothing ->
            defaultImage



-- type alias Node =
--     { node : Maybe Post }
--
--
-- decodeNode : Decoder Node
-- decodeNode =
--     Decode.map Node
--         (field "node" decodePost)
--
--
-- type alias Post =
--     { title : Maybe String
--     , content : Maybe String
--     , featuredImage : Maybe FeaturedImage
--     }
--
--
-- decodePost : Decoder Post
-- decodePost =
--     Decode.map3 Post
--         (field "title" string)
--         (field "content" string)
--         (field "featuredImage" decodeFeaturedImage)
--
--
-- type alias Posts =
--     { edges : List Node
--     }
--
--
-- decodePosts : Decoder Posts
-- decodePosts =
--     Decode.map Posts
--         (field "edges" decodeNode)


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
    -> Decoder PageBy
    -> GraphQl.Request Query Variables PageBy
baseRequest =
    GraphQl.query graphqlEndpoint


sendRequest : Cmd Msg
sendRequest =
    baseRequest pageRequest decodePageBy
        |> GraphQl.send GotContent


extractToModel : PageBy -> Model -> Model
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
    = GotContent (Result Error PageBy)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok pageBy) ->
            ( extractToModel pageBy model, Cmd.none )

        GotContent (Err err) ->
            ( { model | content = toString err }, Cmd.none )


renderHtml : String -> Html.Attribute msg
renderHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


view : Model -> Html.Html Msg
view { featuredImage, content } =
    div [ classes [ pa3, sans_serif ], style [ ( "maxWidth", "32rem" ), ( "margin", "auto" ) ] ]
        [ div []
            [ img [ src featuredImage ] []
            ]
        , div [ renderHtml content ] []
        ]
