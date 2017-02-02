module Main exposing (..)

import Material
import Navigation
import Msg exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)


init : Flags -> Navigation.Location -> ( AppModel, Cmd Msg )
init flags location =
  urlChange (Err flags) location


main : Program Flags AppModel Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = Material.subscriptions Mdl
        }
