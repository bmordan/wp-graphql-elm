module Articles exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, int, string, list, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Http exposing (Error)
import Html exposing (text, div, button, a, strong, img, span)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Config exposing (graphqlEndpoint)
import GraphQl exposing (Operation, Variables, Query, Named)
import Elements exposing (navbar, footer, strToHtml, defaultImg)
import Tachyons exposing (..)
import Tachyons.Classes
    exposing
        ( pa2
        , db
        , mv3
        , mt2
        , center
        , w_100
        , ba
        , b__dark_gray
        , link
        , flex
        , justify_start
        , justify_end
        , items_center
        , flex_none
        , flex_auto
        , mr2
        , br_100
        , w2
        , h2
        , tr
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
    , title : String
    , excerpt : String
    , featuredImage : Maybe FeaturedImage
    , author : Author
    , commentCount : Maybe Int
    }


type alias Author =
    { name : String
    , avatar : Avatar
    }


type alias Avatar =
    { url : Maybe String
    }


type alias FeaturedImage =
    { sourceUrl : String
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
        |> required "title" string
        |> required "excerpt" string
        |> required "featuredImage" (nullable decodeFeaturedImage)
        |> required "author" decodeAuthor
        |> required "commentCount" (nullable int)


decodeAuthor : Decoder Author
decodeAuthor =
    decode Author
        |> required "name" string
        |> required "avatar" decodeAvatar


decodeAvatar : Decoder Avatar
decodeAvatar =
    decode Avatar
        |> required "url" (nullable string)


decodeFeaturedImage : Decoder FeaturedImage
decodeFeaturedImage =
    decode FeaturedImage
        |> required "sourceUrl" string


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
                                , GraphQl.field "title"
                                , GraphQl.field "excerpt"
                                , GraphQl.field "featuredImage"
                                    |> GraphQl.withSelectors
                                        [ GraphQl.field "sourceUrl"
                                        ]
                                , GraphQl.field "author"
                                    |> GraphQl.withSelectors
                                        [ GraphQl.field "name"
                                        , GraphQl.field "avatar"
                                            |> GraphQl.withSelectors
                                                [ GraphQl.field "url"
                                                ]
                                        ]
                                , GraphQl.field "commentCount"
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


renderFeaturedImage : Maybe FeaturedImage -> Html.Html Msg
renderFeaturedImage featuredImage =
    case featuredImage of
        Just image ->
            img [ src image.sourceUrl ] []

        Nothing ->
            defaultImg


renderAuthorAvatar : Avatar -> Html.Html Msg
renderAuthorAvatar avatar =
    case avatar.url of
        Just url ->
            img [ classes [ br_100, w2, h2 ], src url ] []

        Nothing ->
            defaultImg


renderCommentCount : Maybe Int -> Html.Html Msg
renderCommentCount commentCount =
    case commentCount of
        Just count ->
            span [] [ text ((toString count) ++ " Comments") ]

        Nothing ->
            span [] [ text "0 Comments" ]


renderSmallAuthor : Post -> Html.Html Msg
renderSmallAuthor { author, commentCount } =
    div [ classes [ flex, items_center, justify_end ] ]
        [ renderCommentCount commentCount
        , div [ classes [ flex_auto, mr2, tr ] ] [ text author.name ]
        , renderAuthorAvatar author.avatar
        ]


renderPost : Node -> Html.Html Msg
renderPost { node } =
    div [ classes [ db, mv3, ba, b__dark_gray ] ]
        [ renderFeaturedImage node.featuredImage
        , div [ classes [ pa2, mt2 ] ]
            [ a [ href ("/article.html#" ++ node.slug), classes [ link ] ]
                [ strong [] [ text node.title ]
                ]
            , div [ strToHtml node.excerpt ] []
            , renderSmallAuthor node
            ]
        ]


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
