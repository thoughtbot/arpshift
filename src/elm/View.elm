module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, href, name, type_)
import Html.Events exposing (onClick)
import Lane exposing (Lane)
import Model exposing (Model, Msg(..))
import Music
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
        [ nav [ class "header" ]
            [ div [ class "logo" ]
                [ span [ class "logo-left" ] [ text "ARP" ]
                , span [ class "logo-right" ] [ text "SHIFT" ]
                ]
            , button [ class "play", onClick TogglePlay ] [ text "Play" ]
            ]
        , main_ [ class "main" ] <| [ topLabels ] ++ (List.map viewLane <| withIndex model.lanes)
        ]


topLabels : Html a
topLabels =
    section [ class "top-labels" ]
        [ div [ class "root-label" ] [ text "Root" ]
        , div [ class "pitch-label" ] [ text "Pitch" ]
        , div [ class "transposed-label" ] [ text "Transposed" ]
        , div [ class "bpm-label" ] [ text "120BPM" ]
        ]


viewLane : ( Lane, Int ) -> Html Msg
viewLane ( lane, laneIndex ) =
    let
        pitchNode v =
            let
                currentClass =
                    case Music.compareOffset (Lane.laneOffset lane) v of
                        GT ->
                            "pitch-node-on"

                        EQ ->
                            "pitch-node-active"

                        LT ->
                            "pitch-node-off"
            in
            span [ class currentClass, onClick <| SetOffsetOnLane lane v ] []

        notes =
            SelectList.toListWithPosition <| Lane.laneNotes lane

        halfStepValues =
            List.map Music.HalfStep <| List.range 0 11

        viewNote ( ( note, pos ), index ) =
            let
                currentClass =
                    case ( pos == SelectList.Selected, Lane.enabled index lane ) of
                        ( True, _ ) ->
                            "note-selected"

                        ( _, True ) ->
                            "note-enabled"

                        _ ->
                            "note-disabled"

                playingClass =
                    if note then
                        "playing-node-on"

                    else
                        "playing-node-off"

                inactiveTimeNode =
                    div [ class "time-node", onClick <| ToggleLoopBack lane index ] []

                activeTimeNode =
                    div
                        [ class "time-node-active"
                        , onClick <| ToggleLoopBack lane index
                        ]
                        [ div [ class "time-bar-active" ] [] ]
            in
            div
                [ class currentClass
                , onClick <| ToggleNoteInLane lane index
                ]
                [ if Lane.currentLoopPosition index lane then
                    activeTimeNode

                  else
                    inactiveTimeNode
                , div [ class playingClass ] []
                ]
    in
    section [ class "lane" ]
        [ div [ class "root" ] [ text "A" ]
        , div [ class "pitch" ] (List.map pitchNode halfStepValues)
        , div [ class "transposed" ] [ text "Bb" ]
        , div [ class "notes" ] (List.map viewNote <| withIndex notes)
        ]
