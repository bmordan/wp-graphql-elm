module Home exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, at, field, maybe, int, string, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional, custom, requiredAt)
import Http exposing (..)
import Html exposing (a, text, img, h1, h2, p, div, span, input, button)
import Html.Attributes exposing (style, classList, src, placeholder, value)
import Html.Events exposing (onClick, onInput)
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


type Msg
    = GotContent (Result Error Data)
    | Term String
    | GotSearch (Result Error Search)
    | GoSearch


type alias Search =
    { posts : Edges
    }


type alias Edges =
    { edges : List Node
    }


type alias Node =
    { node : Post
    }


type alias Post =
    { title : String
    , slug : String
    , excerpt : String
    }


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


type alias QueryArgs =
    { search : String
    }


type alias Model =
    { content : Maybe String
    , featuredImage : Maybe FeaturedImage
    , loading : Bool
    , term : String
    }


decodeSearch : Decoder Search
decodeSearch =
    decode Search
        |> required "posts" decodeEdges


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
        |> required "title" string
        |> required "slug" string
        |> required "excerpt" string


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
        |> required "sourceUrl" string


pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "query"
        [ GraphQl.field "pageBy"
            |> GraphQl.withArgument "uri" (GraphQl.string "home")
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


searchQuery : String -> Operation Query Variables
searchQuery term =
    GraphQl.named "searchQuery"
        [ GraphQl.field "posts"
            |> GraphQl.withArgument "where" (GraphQl.variable "search")
            |> GraphQl.withSelectors
                [ GraphQl.field "edges"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "node"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "slug"
                                ]
                        ]
                ]
        ]
        |> GraphQl.withVariables [ ( "search", "QueryArgs" ) ]


searchRequest :
    Operation Query Variables
    -> Decoder Search
    -> GraphQl.Request Query Variables Search
searchRequest =
    GraphQl.query graphqlEndpoint


sendSearch : String -> Cmd Msg
sendSearch term =
    let
        searchTerm =
            { search = term }
    in
        searchRequest (searchQuery term) decodeSearch
            |> GraphQl.addVariables [ ( "search", (Encode.encode 0 searchTerm) ) ]
            |> GraphQl.send GotSearch


extractToModel : Data -> Model -> Model
extractToModel { pageBy } model =
    { model
        | content = pageBy.content
        , featuredImage = pageBy.featuredImage
        , loading = False
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing True "", sendRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok data) ->
            ( extractToModel data model, Cmd.none )

        GotContent (Err err) ->
            ( model, Cmd.none )

        Term search ->
            ( { model | term = search }, Cmd.none )

        GotSearch (Ok search) ->
            ( { model | term = "" }, Cmd.none )

        GotSearch (Err err) ->
            ( { model | term = "search error" }, Cmd.none )

        GoSearch ->
            ( model, sendSearch model.term )


renderHtml : String -> Html.Attribute msg
renderHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


renderFeaturedImage : Maybe FeaturedImage -> Html.Html Msg
renderFeaturedImage feature =
    case feature of
        Just val ->
            img [ src val.sourceUrl ] []

        Nothing ->
            Elements.defaultImg


searchBox : Model -> Html.Html Msg
searchBox model =
    div []
        [ input [ onInput Term, placeholder "search our articles", value model.term ] []
        , button [ onClick GoSearch ] [ text "Search" ]
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ navbar
        , div []
            [ renderFeaturedImage model.featuredImage
            , div [ renderHtml (Maybe.withDefault "" model.content) ] []
            , searchBox model
            ]
        , footer model.loading
        ]
