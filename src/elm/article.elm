module Article exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, int, string, list, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional, requiredAt)
import Http exposing (Error)
import Html exposing (text, div, button, a, strong, img, span, p)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Dom exposing (Error)
import Dom.Scroll exposing (toTop)
import Task
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
        , mr2
        , br_100
        , items_center
        , bg_light_gray
        , bg_dark_gray
        , white
        , f3
        , overflow_y_scroll
        , mb4
        , h2
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


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel location, postsRequest "null" )


type Msg
    = Slug Navigation.Location
    | GotPosts (Result Http.Error PostsData)
    | GotPost (Result Http.Error PostBy)
    | Scroll


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
    , author : Author
    , featuredImage : Maybe FeaturedImage
    }


type alias FeaturedImage =
    { sourceUrl : String
    }


type alias Author =
    { name : String
    , bio : String
    , avatar : String
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
        |> required "author" decodeAuthor
        |> required "featuredImage" (nullable decodeFeaturedImage)


decodeFeaturedImage : Decoder FeaturedImage
decodeFeaturedImage =
    decode FeaturedImage
        |> required "sourceUrl" string


decodeAuthor : Decoder Author
decodeAuthor =
    decode Author
        |> required "name" string
        |> required "bio" string
        |> requiredAt [ "avatar", "url" ] string


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
                , GraphQl.field "author"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "name"
                        , GraphQl.field "description"
                            |> GraphQl.withAlias "bio"
                        , GraphQl.field "avatar"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "url"
                                ]
                        ]
                , GraphQl.field "featuredImage"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "sourceUrl"
                        ]
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


updatePost : Model -> PostBy -> Model
updatePost model postdata =
    let
        slug =
            postdata.postBy.slug

        newModel =
            { model
                | post = Just postdata.postBy
                , prev = maybeLink model subtract1
                , next = maybeLink model add1
            }
    in
        newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slug location ->
            let
                slug =
                    String.dropLeft 1 location.hash
            in
                ( { model | slug = maybeSlug location }, postRequest slug )

        GotPosts (Ok postsdata) ->
            updatePosts model postsdata

        GotPosts (Err err) ->
            ( model, Cmd.none )

        GotPost (Ok postdata) ->
            ( updatePost model postdata, Task.attempt (always Scroll) <| toTop (Maybe.withDefault "elm-root" model.slug) )

        GotPost (Err err) ->
            ( model, Cmd.none )

        Scroll ->
            ( model, Cmd.none )


viewAuthor : Author -> Html.Html Msg
viewAuthor author =
    div [ classes [ flex, items_center, bg_light_gray, pa2 ] ]
        [ img [ src author.avatar, classes [ mr2, br_100 ] ] []
        , div [ classes [ flex_auto ] ]
            [ strong [] [ text author.name ]
            , p [] [ text author.bio ]
            ]
        ]


viewFeaturedImage : Maybe FeaturedImage -> Html.Html Msg
viewFeaturedImage featured =
    case featured of
        Just val ->
            img [ src val.sourceUrl ] []

        Nothing ->
            defaultImg


viewPost : Model -> Html.Html Msg
viewPost model =
    case model.post of
        Just post ->
            div [ Html.Attributes.id post.slug, classes [ overflow_y_scroll ], style [ ( "height", "95vh" ) ] ]
                [ viewFeaturedImage post.featuredImage
                , div
                    [ classes [ bg_dark_gray, white, f3, pa2 ]
                    , strToHtml post.title
                    ]
                    []
                , viewAuthor post.author
                , div [ strToHtml post.content ] []
                ]

        Nothing ->
            div [] []


viewPrevLink : Maybe String -> Html.Html Msg
viewPrevLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href ("#" ++ link), classes [ flex_auto, justify_start ] ] [ text ("<- " ++ link) ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/articles"), classes [ flex_auto, justify_start ] ] [ text "<- back to articles" ]


viewNextLink : Maybe String -> Html.Html Msg
viewNextLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href ("#" ++ link), classes [ justify_end ] ] [ text (link ++ " ->") ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/articles"), classes [ justify_end ] ] [ text "back to articles ->" ]


viewLinks : Model -> Html.Html Msg
viewLinks model =
    div [ classes [ flex, h2 ] ]
        [ viewPrevLink model.prev
        , viewNextLink model.next
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ viewPost model
        , viewLinks model
        ]
