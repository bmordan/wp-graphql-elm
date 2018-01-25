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
    , posts : Edges
    }


type alias Page =
    { content : Maybe String
    , featuredImage : Maybe FeaturedImage
    }


type alias FeaturedImage =
    { sourceUrl : String
    }


type alias Edges =
    { edges : List Node
    }


type alias Node =
    { node : Post
    }


type alias Post =
    { id : String
    , slug : String
    , title : String
    , excerpt : Maybe String
    }


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "pageBy" decodePage
        |> required "posts" decodeEdges


decodePage : Decoder Page
decodePage =
    decode Page
        |> required "content" (nullable string)
        |> required "featuredImage" (nullable decodeFeaturedImage)


decodeFeaturedImage : Decoder FeaturedImage
decodeFeaturedImage =
    decode FeaturedImage
        |> optional "sourceUrl" string defaultImage


decodeEdges : Decoder Edges
decodeEdges =
    decode Edges
        |> required "edges" (Decode.list decodeNode)


decodeNode : Decoder Node
decodeNode =
    decode Node
        |> required "node" decodePost


decodePost : Decoder Post
decodePost =
    decode Post
        |> required "id" string
        |> required "slug" string
        |> required "title" string
        |> required "excerpt" (nullable string)


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


extractPostContent : Maybe String -> String
extractPostContent content =
    case content of
        Just val ->
            val

        Nothing ->
            "Can't find content"


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
        , GraphQl.field "posts"
            |> GraphQl.withSelectors
                [ GraphQl.field "edges"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "node"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "id"
                                , GraphQl.field "slug"
                                , GraphQl.field "title"
                                , GraphQl.field "excerpt"
                                ]
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
extractToModel { pageBy, posts } model =
    { model
        | content = extractPageContent pageBy.content
        , featuredImage = extractPageFeaturedImage pageBy.featuredImage
        , posts = List.map (\post -> post.node) posts.edges
    }


type alias Model =
    { content : String
    , featuredImage : String
    , posts : List Post
    }


initModel : Model
initModel =
    Model "" "" []


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


renderPost : Post -> Html.Html Msg
renderPost post =
    a [ Html.Attributes.href ("#" ++ post.slug) ]
        [ div []
            [ Html.h2 [] [ text post.title ]
            , div [ renderHtml (Maybe.withDefault "" post.excerpt) ] []
            ]
        ]


view : Model -> Html.Html Msg
view { featuredImage, content, posts } =
    div [ classes [ pa3, sans_serif ], style [ ( "maxWidth", "32rem" ), ( "margin", "auto" ) ] ]
        [ div []
            [ img [ src featuredImage ] []
            ]
        , div [ renderHtml content ] []
        , div [] (List.map renderPost posts)
        ]
