module Msg exposing (..)

import API exposing (..)
import Material
import Http
import Navigation


type Page = Home

type Msg
    = Mdl (Material.Msg Msg)
    | NewPage Page
    | UrlChange Navigation.Location
    | SearchInput String
    | SearchEscape
    | FetchChannelFiles (Result Http.Error (List (Storepathcontent, Storepath)))
