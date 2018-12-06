module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, href, name, type_)
import Html.Events exposing (onClick)
import Lane exposing (Lane)
import Model exposing (Model, Msg(..))
import SelectList


withIndex : List a -> List ( a, Int )
withIndex xs =
    let
        range =
            List.range 0 <| List.length xs
    in
    List.map2 Tuple.pair xs range


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Arpshift" ]
        , button [ onClick TogglePlay ] [ text "Toggle play" ]
        , div [] (List.map viewLane <| withIndex model.lanes)
        ]


viewLane : ( Lane, Int ) -> Html Msg
viewLane ( lane, laneIndex ) =
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

        halfStepValues =
            List.range 0 5

        viewNote ( ( note, pos ), index ) =
            li
                [ classList
                    [ ( "current", pos == SelectList.Selected )
                    , ( "enabled", Lane.enabled index lane )
                    ]
                ]
                [ button [ onClick <| ToggleNoteInLane lane index ]
                    [ text <| noteToString note
                    ]
                , button [ onClick <| ToggleLoopBack lane index ]
                    [ text "Toggle loop"
                    ]
                ]

        viewRadio v =
            input
                [ name <| "lane-radio-" ++ String.fromInt laneIndex
                , checked <| Lane.laneOffset lane == v
                , type_ "radio"
                , onClick <| SetOffsetOnLane lane v
                ]
                [ text <| String.fromInt v ]

        radios =
            fieldset [] (List.map viewRadio halfStepValues)
    in
    div []
        [ ul [ class "lane" ] <| List.map viewNote (withIndex notes)
        , radios
        ]
