module News exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, at, field, maybe, int, string, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional, custom, requiredAt)
import Http exposing (..)
import Html exposing (a, text, img, h1, h2, p, div, span)
import Html.Attributes exposing (style, classList, src)
import Config exposing (graphqlEndpoint, baseUrl)
import GraphQl exposing (Operation, Variables, Query, Named)
import Navigation
import Tachyons exposing (..)
import Tachyons.Classes exposing (..)
import Debug exposing (log)
import List.Extra exposing (elemIndex, getAt)


main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type Msg
    = UrlChange Navigation.Location
    | GotPosts (Result Error Data)
    | GotPost (Result Error PostBy)


type alias PostBy =
    { postBy : FullPost
    }


type alias Data =
    { posts : Edges
    }


type alias Edges =
    { edges : List Node
    }


type alias Node =
    { node : ShortPost
    }


type alias ShortPost =
    { id : String
    , title : String
    , slug : String
    , excerpt : String
    }


type alias FullPost =
    { id : String
    , title : String
    , slug : String
    , content : String
    }


type alias Links =
    ( Maybe String, Maybe String )


type alias Model =
    { hash : Maybe String
    , posts : List ShortPost
    , post : Maybe FullPost
    , prev : Maybe String
    , next : Maybe String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            Model (maybeHash location) [] Nothing Nothing Nothing
    in
        ( model, requestPostsCmd model )


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "posts" decodeEdges


decodePostBy : Decoder PostBy
decodePostBy =
    decode PostBy
        |> required "postBy" decodeFullPost


decodeEdges : Decoder Edges
decodeEdges =
    decode Edges
        |> required "edges" (Decode.list decodeNode)


decodeFullPost : Decoder FullPost
decodeFullPost =
    decode FullPost
        |> required "id" string
        |> required "title" string
        |> required "slug" string
        |> required "content" string


decodeNode : Decoder Node
decodeNode =
    decode Node
        |> required "node" decodeShortPost


decodeShortPost : Decoder ShortPost
decodeShortPost =
    decode ShortPost
        |> required "id" string
        |> required "slug" string
        |> required "title" string
        |> required "excerpt" string


postsRequest : Operation Query Variables
postsRequest =
    GraphQl.named "postsRequest"
        [ GraphQl.field "posts"
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


postRequest : String -> Operation Query Variables
postRequest uri =
    GraphQl.named "pageByRequest"
        [ GraphQl.field "postBy"
            |> GraphQl.withArgument "uri" (GraphQl.string uri)
            |> GraphQl.withSelectors
                [ GraphQl.field "id"
                , GraphQl.field "title"
                , GraphQl.field "slug"
                , GraphQl.field "content"
                ]
        ]
        |> GraphQl.withVariables []


postsBaseRequest :
    Operation Query Variables
    -> Decoder Data
    -> GraphQl.Request Query Variables Data
postsBaseRequest =
    GraphQl.query graphqlEndpoint


postBaseRequest :
    Operation Query Variables
    -> Decoder PostBy
    -> GraphQl.Request Query Variables PostBy
postBaseRequest =
    GraphQl.query graphqlEndpoint


maybeHash : Navigation.Location -> Maybe String
maybeHash { hash } =
    if String.length hash > 0 then
        Just (String.dropLeft 1 hash)
    else
        Nothing


renderHtml : String -> Html.Attribute msg
renderHtml str =
    (Html.Attributes.property "innerHTML" (Encode.string str))


createShortPostFromNode : Node -> ShortPost
createShortPostFromNode { node } =
    ShortPost node.id node.slug node.title node.excerpt


extractPosts : Edges -> List ShortPost
extractPosts { edges } =
    if List.isEmpty edges then
        []
    else
        List.map createShortPostFromNode edges


add1 : Int -> Int
add1 n =
    n + 1


subtract1 : Int -> Int
subtract1 n =
    n - 1


addPostsToModel : Data -> Model -> Model
addPostsToModel { posts } model =
    { model | posts = extractPosts posts }


addPostToModel : PostBy -> Model -> Model
addPostToModel { postBy } model =
    { model
        | post = Just postBy
        , prev = maybeLink model subtract1
        , next = maybeLink model add1
    }


maybeLink : Model -> (Int -> Int) -> Maybe String
maybeLink model fn =
    let
        posts =
            List.map (\post -> post.slug) model.posts

        hash =
            case model.hash of
                Just val ->
                    val

                Nothing ->
                    ""

        index =
            elemIndex hash posts
    in
        case index of
            Just ind ->
                getAt (fn ind) posts

            Nothing ->
                Nothing


requestPostsCmd : Model -> Cmd Msg
requestPostsCmd model =
    if List.isEmpty model.posts then
        postsBaseRequest postsRequest decodeData
            |> GraphQl.send GotPosts
    else
        Cmd.none


updatePostCmd : Model -> ( Model, Cmd Msg )
updatePostCmd model =
    let
        command =
            case model.hash of
                Just uri ->
                    postBaseRequest (postRequest uri) decodePostBy
                        |> GraphQl.send GotPost

                Nothing ->
                    Cmd.none
    in
        ( model, command )


updateModelWithLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
updateModelWithLocation location model =
    { model | hash = maybeHash location }
        |> updatePostCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            updateModelWithLocation location model

        GotPosts (Ok data) ->
            addPostsToModel data model
                |> updatePostCmd

        GotPosts (Err err) ->
            ( model, Cmd.none )

        GotPost (Ok post) ->
            ( addPostToModel post model, Cmd.none )

        GotPost (Err err) ->
            ( model, Cmd.none )


createShortPostView : ShortPost -> Html.Html Msg
createShortPostView post =
    a [ Html.Attributes.href ("#" ++ post.slug) ]
        [ div []
            [ Html.h2 [] [ text post.title ]
            , div [ renderHtml post.excerpt ] []
            ]
        ]


createFullPostView : Model -> Html.Html Msg
createFullPostView model =
    case model.post of
        Just post ->
            div []
                [ a [ Html.Attributes.href (baseUrl ++ "/news.html#") ] [ text "<- back" ]
                , Html.h2 [] [ text post.title ]
                , div [ renderHtml post.content ] []
                , div [ classes [ flex ] ]
                    [ createPrevLink model.prev
                    , createNextLink model.next
                    ]
                ]

        Nothing ->
            div [] []


createPrevLink : Maybe String -> Html.Html Msg
createPrevLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href (baseUrl ++ "/news.html#" ++ link), classes [ flex_auto, justify_start ] ] [ text ("<- " ++ link) ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/news.html#"), classes [ flex_auto, justify_start ] ] [ text "<- back to news" ]


createNextLink : Maybe String -> Html.Html Msg
createNextLink postLink =
    case postLink of
        Just link ->
            a [ Html.Attributes.href (baseUrl ++ "/news.html#" ++ link), classes [ justify_end ] ] [ text (link ++ " ->") ]

        Nothing ->
            a [ Html.Attributes.href (baseUrl ++ "/news.html#"), classes [ justify_end ] ] [ text "back to news ->" ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div [] []
        , case model.hash of
            Just hash ->
                createFullPostView model

            Nothing ->
                div [] (List.map createShortPostView model.posts)
        ]
