module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, int, map3, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Gamer


type alias Gamer =
    { id : Int
    , nickname : String
    , score : Int
    }


-- Gamers


type alias ListModel =
    { gamerList : List Gamer
    , msg : String
    }



-- Model


type Model
    = Failure
    | Loading
    | Success Gamer
    | SuccessAll (List Gamer)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getSingleGamer )



-- Update


type Msg
    = MorePlease
    | SingleGamerButton
    | DeleteGamer
    | GetAllGamersButton
    | GotGamer (Result Http.Error Gamer)
    | GotAllGamerList (Result Http.Error (List Gamer))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getSingleGamer )

        SingleGamerButton ->
            ( Loading, getSingleGamer )

        DeleteGamer ->
            ( Loading, deleteGamer )

        GetAllGamersButton ->
            ( Loading, getAllGamers )

        GotGamer result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotAllGamerList result ->
            case result of
                Ok url ->
                    ( SuccessAll url, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Gamer configuration" ]
        , hr [] []
        , searchGamer model
        , viewTable model
        ]


searchGamer : Model -> Html Msg
searchGamer model =
    div []
        [ h5 [] [ text "Get gamerlist: " ]
        , button [ onClick GetAllGamersButton ] [ text "Show list!" ]
        , hr [] []
        , h5 [] [ text "Show or remove gamer: " ]
        , input [ type_ "text", placeholder "Search by id" ] []
        , br [] []
        , button [ onClick SingleGamerButton ] [ text "Get gamer!" ]
        , button [ onClick DeleteGamer ] [ text "Remove gamer!" ]
        , hr [] []
        , h5 [] [ text "Create a new gamer here: " ]
        , input [ type_ "text", placeholder "nickname" ] []
        , input [ type_ "text", placeholder "score" ] []
        , br [] []
        , button [ onClick MorePlease ] [ text "Create gamer!" ]
        , hr [] []
        ]


viewTable : Model -> Html Msg
viewTable model =
    case model of
        Failure ->
            div []
                [ text "No gamers found "
                , button [ onClick MorePlease ] [ text "Try again?" ]
                ]

        Loading ->
            text "Loading..."

        Success gamer ->
            div []
                [ h3 [] [ text "List of gamers" ]
                , table []
                    [ tr []
                        [ th [] [ text "id " ]
                        , th [] [ text "nickname " ]
                        , th [] [ text "score " ]
                        ]
                    , tr []
                        [ td [] [ text (String.fromInt gamer.id) ]
                        , td [] [ text gamer.nickname ]
                        , td [] [ text (String.fromInt gamer.score) ]
                        ]
                    ]
                ]

        SuccessAll list ->
            table []
                ([ tr []
                    [ th [] [ text "Id" ]
                    , th [] [ text "Nickname" ]
                    , th [] [ text "Score" ]
                    ]
                 ]
                    ++ List.map showGamer list
                )


showGamer : Gamer -> Html Msg
showGamer gamer =
    tr []
        [ td [] [ text (String.fromInt gamer.id) ]
        , td [] [ text gamer.nickname ]
        , td [] [ text (String.fromInt gamer.score) ]
        ]



-- HTTP

deleteGamer : Cmd Msg
deleteGamer =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:4711/gamer/4"
        , body = Http.emptyBody
        , expect = Http.expectJson GotGamer gamerDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

getSingleGamer : Cmd Msg
getSingleGamer =
    Http.get
        { url = "http://localhost:4711/gamer/1"
        , expect = Http.expectJson GotGamer gamerDecoder
        }


getAllGamers : Cmd Msg
getAllGamers =
    Http.get
        { url = "http://localhost:4711/gamer/"
        , expect = Http.expectJson GotAllGamerList gamerListDecoder
        }



-- JSON Decoders


gamerDecoder : Decoder Gamer
gamerDecoder =
    map3 Gamer (field "id" int) (field "nickname" string) (field "score" int)


gamerListDecoder : Decoder (List Gamer)
gamerListDecoder =
    JD.list gamerDecoder


msgDecoder : Decoder String
msgDecoder =
    field "nickname" string
