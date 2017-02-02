module View exposing (..)

import Html exposing (..)
import List
import Json.Decode as Json
import Maybe
import Material.Scheme
import Material.Color as Color
import Material.Layout as Layout
import Material.Table as Table
import Material.Icon as Icon
import Material.Options as Options
import Material.Footer as Footer
import Material.Textfield as Textfield
import Msg exposing (..)
import Models exposing (..)


view : AppModel -> Html Msg
view model =
    Options.div
        []
        [ Material.Scheme.topWithScheme Color.BlueGrey Color.LightBlue <|
            Layout.render Mdl
                model.mdl
                [ Layout.fixedHeader ]
                { header = []
                , drawer = []
                , tabs = ( [], [ Color.background (Color.color Color.LightBlue Color.A700) ] )
                , main = viewBody model
                }
        ]


viewBody : AppModel -> List (Html Msg)
viewBody model =
        Options.div
            [ Options.css "margin" "30px"
            , Options.css "min-height" "100%"
            ]
            (pageToView model)
            :: [ Footer.mini
                    [ Options.css "position" "absolute"
                    , Options.css "bottom" "-70px"
                    , Options.css "width" "100%"
                    ]
                    { left =
                        Footer.left []
                            [ Footer.logo
                                []
                                []
                            ]
                    , right =
                        Footer.right []
                            [ Footer.logo
                                []
                                []
                            ]
                    }
               ]



pageToView : AppModel -> List (Html Msg)
pageToView model = case model.currentPage of
    Home ->
      [ Options.div
          [ Options.css "margin" "auto"
          , Options.css "width" "400px"] 
          [ Textfield.render 
            Mdl [0] model.mdl
            [ Textfield.label "Search files in Nix packages ..."
            , Textfield.floatingLabel
            , Textfield.text_
            , Options.onInput SearchInput
            , onEscape SearchEscape
            , Textfield.value model.searchString
            , Options.input
                [
                ]
             ] []
           , Icon.view "search"
            [ Options.onClick (SearchInput model.searchString)
              -- TODO: trigger a proper search page
            , Options.css "position" "relative"
            , Options.css "top" "8px"
            , Options.css "right" "28px"
            , Options.css "z-index" "100"
            , Options.css "cursor" "pointer"
            ]
      , Table.table []
        [ Table.thead []
          [ Table.tr []
            [ Table.th [] [ text "Store path" ]
            , Table.th [ ] [ text "File" ]
            , Table.th [ ] [ text "Size" ]
            ]
          ]
        , Table.tbody []
            (model.files |> List.map (\(contents, path) ->
               Table.tr []
                 [ Table.td [] [ text path.storepathPath ]
                 , Table.td [] [ text contents.storepathcontentFile ]
                 , Table.td [ Table.numeric ] [ text (toString (Maybe.withDefault 0 contents.storepathcontentFilesize)) ]
                 ]
               )
            )
        ]
      ]]


onEscape : msg -> Textfield.Property msg
onEscape msg =
    Options.on "keydown" (Json.map (always msg) (Json.int |> Json.andThen isEscape))


isEscape : Int -> Json.Decoder Int
isEscape code =
    case code of
        27 ->
            Json.succeed 27

        _ ->
            Json.fail "not the right key code"
