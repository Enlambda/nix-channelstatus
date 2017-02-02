port module Update exposing (..)

import API exposing (..)
import Http
import Material
import Navigation
import Models exposing (..)
import Msg exposing (..)
import String
import UrlParser exposing (Parser, (</>), map, int, oneOf, s, string, parsePath)


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        NewPage page ->
            ( model, Navigation.newUrl (pageToURL page) )

        UrlChange location ->
          urlChange (Ok model) location

        SearchInput searchString ->
            if String.length searchString < 3
            then ({ model | searchString = searchString}, Cmd.none)
            else ({ model | searchString = searchString}
                 , Http.send FetchChannelFiles (getApiChannelFiles model.backendURL (Just (String.trim searchString))))

        SearchEscape ->
            ({ model | searchString = ""}, Cmd.none)

        FetchChannelFiles result ->
            case result of
                Err msg -> (model, Cmd.none)
                Ok files -> ({ model | files = files }, Cmd.none)

urlChange : Result Flags AppModel -> Navigation.Location ->  ( AppModel, Cmd Msg)
urlChange result location =
    let
        page = Maybe.withDefault Home (parsePath pageParser location)
        model = case result of
            Ok model -> model
            Err flags -> initialModel page flags
    in model ! [ Material.init Mdl
               , title (pageToTitle page)
               ]

pageParser : Parser (Page -> a) a
pageParser =
    oneOf
       [ map Home (s "")
       ]


pageToURL : Page -> String
pageToURL page =
    case page of
        Home ->
            "/"


pageToTitle : Page -> String
pageToTitle page =
    case page of
        Home ->
            ""

-- Ports


port title : String -> Cmd msg
