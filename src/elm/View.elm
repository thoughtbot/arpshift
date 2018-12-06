module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


view : Model -> Html a
view _ =
    div []
        [ h1 [] [ text "Arpshift" ]
        ]
