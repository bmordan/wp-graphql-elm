module App exposing (..)

import Html exposing (Html, div, text, h1, p, span)
import Html.Attributes exposing (..)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)
import Json.Encode as Encode
import Debug exposing (log)


type alias Page =
    { title : Maybe String
    , content : Maybe String
    }


postRequest : Request Query Page
postRequest =
    let
        uri =
            Var.required "uri" .uri Var.string
    in
        extract
            (field "pageBy"
                [ ( "uri", Arg.variable uri ) ]
                (object Page
                    |> with (field "title" [] (nullable string))
                    |> with (field "content" [] (nullable string))
                )
            )
            |> queryDocument
            |> request
                { uri = "contact-us"
                }


connectionNodes :
    ValueSpec NonNull ObjectType result vars
    -> ValueSpec NonNull ObjectType (List result) vars
connectionNodes spec =
    extract
        (field "edges"
            []
            (GraphQL.Request.Builder.list
                (extract
                    (field "node" [] spec)
                )
            )
        )


type alias PageResponse =
    Result GraphQLClient.Error Page


type alias Model =
    { title : String
    , content : String
    , author : String
    , avatar : String
    }


type Msg
    = ReceiveQueryResponse PageResponse


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
sendQueryRequest request =
    GraphQLClient.sendQuery "http://localhost:8000/graphql" request


sendPageQuery : Cmd Msg
sendPageQuery =
    sendQueryRequest postRequest
        |> Task.attempt ReceiveQueryResponse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model "--------" "<p>fetching content ... .   .</p>" "" "http://via.placeholder.com/120x120", sendPageQuery )


extractContentToModel : Page -> Model -> Model
extractContentToModel content model =
    { model
        | title = Maybe.withDefault model.title content.title
        , content = Maybe.withDefault model.content content.content
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveQueryResponse (Ok content) ->
            ( (extractContentToModel content model), Cmd.none )

        ReceiveQueryResponse (Err _) ->
            ( model, Cmd.none )


setHtml : String -> Html.Attribute msg
setHtml txt =
    Html.Attributes.property "innerHTML" (Encode.string txt)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


mainStyle : Html.Attribute msg
mainStyle =
    style
        [ ( "width", "90%" )
        , ( "margin", "1rem auto" )
        , ( "fontFamily", "verdana" )
        ]


view : Model -> Html Msg
view { title, content } =
    div [ mainStyle ]
        [ h1 [ setHtml title ] []
        , p [ setHtml content ] []
        ]
