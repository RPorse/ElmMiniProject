module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http
import Json.Decode as Json


--Model

type alias Model =
    { gamerList : GamerList
    , gamers : List Gamer
    }

type alias GamerList =
    { name : String }

type alias Gamer =
    { id : Int
    , nickname : String 
    , score : Int
    }

init : ( Model, cmd Msg)
init =
    ( Model (GamerList "Gamers!") [], Cmd.none )


--Update

type Msg   
    = OpenGamer (Result Http.Error (List Gamer))
    | GetGamer
    | UpdateGamer String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
        case msg of
            OpenGamer   (Ok json) -> 
                        ({model | gamers = json}, Cmd.none)                
        
            OpenGamer   (Err e) -> 
                        (Debug.log (toString e) model, Cmd.none)
            GetGamer    ->
                        (model, getInfo model.GamerList.name)

            UpdateGamer string ->
                        ({model | gamerlist = (updateSElection string)}, Cmd.none)

updateSelection : String -> Subreddit
updateSelection string =
    GamerList string
                
        

--View
view : Model -> Html Msg
view model =
    div []
        [div[]
            [ input [ type_ "text", placeholder "Gamerlist", onInput UpdateGamer][]
            , button [onClick GetGamer][ text "Go!"]
            , h3 [] [ text model.gamerlist.name ]
            , h3 [] [text <| "http://localhost:8000/" ++ model.gamerlist.name]
            , div [] <| List.map postView model.gamers
            ]
        ]

gamerView : Post -> Html Msg
gamerView post =
    div []
        [ a [ href post.url] [ text gamer.nickname]
        ]

        
--Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

--Commands

getInfo : String -> Cmd Msg
getInfo string =
    let
        url =
            "http://localhost:8000/" ++ string ++ "/.json"
        
        req =
            Http.get url decodeLocalhost --????
    
    in
        Http.send OpenGamer req

--Json

decodeHost : Json.Decoder (List Gamer)
decodeHost =
    Json.at [ "data", "children"] (Json.list decodeGamer)

decodeGamer : Json.Decoder Post
decodeGamer =
    Json.map3 Post
        (Json.at ["data", "ID"] Json.int)
        (Json.at ["data", "Nickname"] Json.string)
        (Json.at ["data", "Score"] Json.int)

main =
    Html.program
        { init = init
        , update = update
        , view = view
        ,subscriptions = subscriptions
        }