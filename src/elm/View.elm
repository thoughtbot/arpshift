module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Lane exposing (Lane)
import Model exposing (Model, Msg(..))
import SelectList


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Arpshift" ]
        , button [ onClick TogglePlay ] [ text "Toggle play" ]
        , div [] (List.map viewLane model.lanes)
        ]


viewLane : Lane -> Html Msg
viewLane lane =
    let
        notes =
            SelectList.toListWithPosition <| Lane.laneNotes lane

        range =
            List.range 1 <| List.length notes

        notesWithIds =
            List.map2 Tuple.pair notes range

        noteToString note =
            if note == True then
                "True"

            else
                "False"

        viewNote ( ( note, pos ), index ) =
            li
                [ classList
                    [ ( "current", pos == SelectList.Selected )
                    , ( "enabled", Lane.enabled index lane )
                    ]
                ]
                [ button [ onClick <| ToggleNoteInLane lane (index - 1) ]
                    [ text <| noteToString note
                    ]
                , button [ onClick <| ToggleLoopBack lane index ]
                    [ text "Toggle loop"
                    ]
                ]
    in
    ul [ class "lane" ] <| List.map viewNote notesWithIds
