module Models exposing (..)

import API exposing (..)
import Material
import Msg exposing (..)


type alias Flags =
    { backendURL : String
    }


type alias AppModel =
    { mdl : Material.Model
    , searchString : String
    , backendURL : String
    , currentPage : Page
    , files : List (Storepathcontent, Storepath)
    }


initialModel : Page -> Flags -> AppModel
initialModel page flags =
  { mdl = Material.model
  , backendURL = flags.backendURL
  , currentPage = page
  , searchString = ""
  , files = []
  }
