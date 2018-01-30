module Article exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, int, string, list, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional, requiredAt)
import Http exposing (Error)
import Html exposing (text, div, button, a, strong, img, span)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Navigation
import List.Extra exposing (elemIndex, getAt)
import Config exposing (graphqlEndpoint, baseUrl)
import GraphQl exposing (Operation, Variables, Query, Named)
import Elements exposing (navbar, footer, strToHtml, defaultImg)
import Tachyons exposing (..)
import Tachyons.Classes
    exposing
        ( pa2
        , flex
        , justify_end
        , justify_start
        , flex_none
        , flex_auto
        )


main =
    Navigation.program Slug
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


initModel : Navigation.Location -> Model
initModel location =
    { post = Nothing
    , posts = []
    , prev = Nothing
    , next = Nothing
    , slug = maybeSlug location
    }



-- initCmds : String -> String -> Cmd Msg
-- initCmds cursor slug =
--     Cmd.map
--         [ postsRequest cursor
--         , postRequest slug
--         ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel location, postsRequest "null" )


type Msg
    = Slug Navigation.Location
    | GotPosts (Result Error PostsData)
    | GotPost (Result Error PostBy)


type alias PostsData =
    { posts : Posts
    }


type alias Posts =
    { pageInfo : PageInfo
    , edges : List PostLabel
    }


type alias PageInfo =
    { hasNextPage : Bool
    , endCursor : Maybe String
    }


type alias PostLabel =
    { slug : String
    , title : String
    }


type alias PostBy =
    { postBy : Post
    }


type alias Post =
    { slug : String
    , title : String
    , content : String
    }


type alias Model =
    { post : Maybe Post
    , posts : List PostLabel
    , prev : Maybe String
    , next : Maybe String
    , slug : Maybe String
    }


decodePostsData : Decoder PostsData
decodePostsData =
    decode PostsData
        |> required "posts" decodePosts


decodePosts : Decoder Posts
decodePosts =
    decode Posts
        |> required "pageInfo" decodePageInfo
        |> required "edges" (Decode.list decodePostLabel)


decodePageInfo : Decoder PageInfo
decodePageInfo =
    decode PageInfo
        |> required "hasNextPage" bool
        |> required "endCursor" (nullable string)


decodePostLabel : Decoder PostLabel
decodePostLabel =
    decode PostLabel
        |> requiredAt [ "node", "slug" ] string
        |> requiredAt [ "node", "title" ] string


decodePostBy : Decoder PostBy
decodePostBy =
    decode PostBy
        |> required "postBy" decodePost


decodePost : Decoder Post
decodePost =
    decode Post
        |> required "slug" string
        |> required "title" string
        |> required "content" string


postsQuery : String -> Operation Query Variables
postsQuery cursor =
    GraphQl.named "postsQuery"
        [ GraphQl.field "posts"
            |> GraphQl.withArgument "first" (GraphQl.int 100)
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
                                ]
                        ]
                ]
        ]
        |> GraphQl.withVariables []


basePostsRequest :
    Operation Query Variables
    -> Decoder PostsData
    -> GraphQl.Request Query Variables PostsData
basePostsRequest =
    GraphQl.query graphqlEndpoint


postsRequest : String -> Cmd Msg
postsRequest cursor =
    basePostsRequest (postsQuery cursor) decodePostsData
        |> GraphQl.send GotPosts


postQuery : String -> Operation Query Variables
postQuery slug =
    GraphQl.named "postQuery"
        [ GraphQl.field "postBy"
            |> GraphQl.withArgument "slug" (GraphQl.string slug)
            |> GraphQl.withSelectors
                [ GraphQl.field "slug"
                , GraphQl.field "title"
                , GraphQl.field "content"
                ]
        ]
        |> GraphQl.withVariables []


basePostRequest :
    Operation Query Variables
    -> Decoder PostBy
    -> GraphQl.Request Query Variables PostBy
basePostRequest =
    GraphQl.query graphqlEndpoint


postRequest : String -> Cmd Msg
postRequest slug =
    basePostRequest (postQuery slug) decodePostBy
        |> GraphQl.send GotPost


updatePosts : Model -> PostsData -> ( Model, Cmd Msg )
updatePosts model { posts } =
    let
        hasNextPage =
            posts.pageInfo.hasNextPage

        cursor =
            Maybe.withDefault "null" posts.pageInfo.endCursor

        newModel =
            { model | posts = List.append model.posts posts.edges }

        slug =
            case model.slug of
                Just slug ->
                    slug

                Nothing ->
                    ""

        nextCmd =
            if hasNextPage then
                postsRequest cursor
            else
                postRequest slug
    in
        ( newModel, nextCmd )


add1 : Int -> Int
add1 n =
    n + 1


subtract1 : Int -> Int
subtract1 n =
    n - 1


maybeSlug : Navigation.Location -> Maybe String
maybeSlug { hash } =
    if String.length hash > 0 then
        Just (String.dropLeft 1 hash)
    else
        Nothing


maybeLink : Model -> (Int -> Int) -> Maybe String
maybeLink model fn =
    let
        posts =
            List.map (\post -> post.slug) model.posts

        slug =
            case model.slug of
                Just val ->
                    val

                Nothing ->
                    ""

        index =
            elemIndex slug posts
    in
        case index of
            Just ind ->
                getAt (fn ind) posts

            Nothing ->
                Nothing


updatePost : Model -> PostBy -> ( Model, Cmd Msg )
updatePost model postdata =
    let
        newModel =
            { model
                | post = Just postdata.postBy
                , prev = maybeLink model subtract1
                , next = maybeLink model add1
            }
    in
        ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slug location ->
            ( { model | slug = maybeSlug location }, postRequest (String.dropLeft 1 location.hash) )

        GotPosts (Ok postsdata) ->
            updatePosts model postsdata

        GotPosts (Err err) ->
            ( model, Cmd.none )

        GotPost (Ok postdata) ->
            updatePost model postdata

        GotPost (Err err) ->
            ( model, Cmd.none )


viewPost : Model -> Html.Html Msg
viewPost model =
    case model.post of
        Just post ->
            div []
                [ Html.h1 [] [ text post.title ]
                , div [ strToHtml post.content ] []
                , div [ classes [ flex ] ]
                    [ viewPrevLink model.prev
                    , viewNextLink model.next
                    ]
                ]

        Nothing ->
            div [] []


viewPrevLink : Maybe String -> Html.Html Msg
viewPrevLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href (baseUrl ++ "/article.html#" ++ link), classes [ flex_auto, justify_start ] ] [ text ("<- " ++ link) ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/articles"), classes [ flex_auto, justify_start ] ] [ text "<- back to articles" ]


viewNextLink : Maybe String -> Html.Html Msg
viewNextLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href (baseUrl ++ "/article.html#" ++ link), classes [ justify_end ] ] [ text (link ++ " ->") ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/articles"), classes [ justify_end ] ] [ text "back to articles ->" ]


view : Model -> Html.Html Msg
view model =
    div []
        [ viewPost model
        ]
