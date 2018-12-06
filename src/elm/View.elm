module View exposing (view)

import Html exposing (..)
import Model exposing (Model)


view : Model -> Html a
view _ =
    h1 [] [ text "Arpshift" ]
