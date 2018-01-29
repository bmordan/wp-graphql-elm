module Articles exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, int, string, list, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required)
import Http exposing (Error)
import Html exposing (text, div, button)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Config exposing (graphqlEndpoint)
import GraphQl exposing (Operation, Variables, Query, Named)
import Elements exposing (navbar, footer)
import Tachyons exposing (..)
import Tachyons.Classes
    exposing
        ( pa2
        , db
        , mv3
        , center
        , w_100
        )


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type Msg
    = GotPosts (Result Error Data)
    | GetPosts String


type alias Data =
    { posts : Posts }


type alias Posts =
    { pageInfo : PageInfo
    , edges : List Node
    }


type alias PageInfo =
    { hasNextPage : Bool
    , endCursor : Maybe String
    }


type alias Edges =
    { edges : List Node
    }


type alias Node =
    { node : Post
    }


type alias Post =
    { slug : String
    }


type alias Model =
    { loading : Bool
    , hasNextPage : Bool
    , nextCursor : String
    , posts : List Node
    }


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "posts" decodePosts


decodePosts : Decoder Posts
decodePosts =
    decode Posts
        |> required "pageInfo" decodePageInfo
        |> required "edges" (Decode.list decodeNode)


decodePageInfo : Decoder PageInfo
decodePageInfo =
    decode PageInfo
        |> required "hasNextPage" bool
        |> required "endCursor" (nullable string)


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
        |> required "slug" string


postsQuery : String -> Operation Query Variables
postsQuery cursor =
    GraphQl.named "postsQuery"
        [ GraphQl.field "posts"
            |> GraphQl.withArgument "first" (GraphQl.int 4)
            |> GraphQl.withArgument "after" (GraphQl.string cursor)
            |> GraphQl.withSelectors
                [ GraphQl.field "pageInfo"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "hasNextPage"
                        , GraphQl.field "endCursor"
                        ]
                , GraphQl.field "edges"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "node"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "slug"
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


sendPostsQuery : String -> Cmd Msg
sendPostsQuery cursor =
    baseRequest (postsQuery cursor) decodeData
        |> GraphQl.send GotPosts


init : ( Model, Cmd Msg )
init =
    ( Model True False "null" [], (sendPostsQuery "null") )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts (Ok data) ->
            ( updateModelWithPosts model data, Cmd.none )

        GotPosts (Err err) ->
            ( { model | loading = False }, Cmd.none )

        GetPosts cursor ->
            ( { model | loading = True }, sendPostsQuery cursor )


updateModelWithPosts : Model -> Data -> Model
updateModelWithPosts model data =
    { model
        | loading = False
        , hasNextPage = data.posts.pageInfo.hasNextPage
        , nextCursor = Maybe.withDefault "" data.posts.pageInfo.endCursor
        , posts = List.append model.posts data.posts.edges
    }


renderPost : Node -> Html.Html Msg
renderPost { node } =
    div [ classes [ db, mv3 ] ] [ text node.slug ]


renderPosts : List Node -> Html.Html Msg
renderPosts nodes =
    if List.isEmpty nodes then
        div [] [ text "no articles" ]
    else
        div [] (List.map renderPost nodes)


renderMoreBtn : Model -> Html.Html Msg
renderMoreBtn model =
    let
        btnClasses =
            [ center, mv3, w_100 ]
    in
        if model.hasNextPage then
            button [ classes btnClasses, onClick (GetPosts model.nextCursor) ] [ text "fetch more articles" ]
        else
            button [ classes btnClasses ] [ text "you have all the articles" ]


view : Model -> Html.Html Msg
view model =
    div []
        [ navbar
        , div [ classes [ pa2 ] ]
            [ div [] [ (renderPosts model.posts) ]
            , div [] [ renderMoreBtn model ]
            ]
        , footer model.loading
        ]
