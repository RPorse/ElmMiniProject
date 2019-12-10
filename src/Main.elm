module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Http
import Json.Decode as Json
import Material
import Material.Scheme as Scheme
import Material.List as Lists
import Material.Layout as Layout 
import Material.Button as button
import Material.Textfield as Textfield 
import Material.Color as Color 
import Material.Options as Options exposing (css)


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

init : ( Model, Cmd Msg)
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
        
  --          OpenGamer   (Err e) -> 
 --                       (Debug.log (toString e) model, Cmd.none)
            GetGamer    ->
                        (model, getInfo model.gamerList.name)

            UpdateGamer string ->
                        ({model | gamerlist = (updateSelection string)}, Cmd.none)

updateSelection : String -> GamerList
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
            , div [] <| List.map gamerView model.gamers
            ]
        ]

gamerView : Gamer -> Html Msg
gamerView gamer =
    div []
        [ a [ href gamer.url] [ text gamer.nickname]
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

decodeGamer : Json.Decoder Gamer
decodeGamer =
    Json.map3 Gamer
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