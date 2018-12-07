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
            , playPauseButton model
            ]
        , main_ [ class "main" ] <| [ topLabels ] ++ (List.map viewLane <| withIndex model.lanes)
        ]


playPauseButton : Model -> Html Msg
playPauseButton model =
    let
        buttonClass =
            if Model.shouldPlay model then
                "pause"

            else
                "play"

        renderText =
            if Model.shouldPlay model then
                "Pause"

            else
                "Play"
    in
    button [ class buttonClass, onClick TogglePlay ] [ span [ class "icon" ] [], text renderText ]


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
                    case ( note, Lane.enabled index lane ) of
                        ( False, True ) ->
                            "note-enabled"

                        ( True, True ) ->
                            "note-selected"

                        _ ->
                            "note-disabled"

                playingClass =
                    if pos == SelectList.Selected then
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
        [ div [ class "root" ] [ text <| noteToString <| Lane.laneNote lane ]
        , div [ class "pitch" ] (List.map pitchNode halfStepValues)
        , div [ class "transposed" ] [ text <| noteToString <| Lane.laneTransposedNote lane ]
        , div [ class "notes" ] (List.map viewNote <| withIndex notes)
        ]


noteToString : Music.Note -> String
noteToString ( degree, octave ) =
    let
        displayOctave =
            case octave of
                Music.Three ->
                    "3"

                Music.Four ->
                    "4"

                Music.Five ->
                    "5"

        displayDegree =
            case degree of
                Music.C ->
                    "C"

                Music.Cs ->
                    "Db"

                Music.D ->
                    "D"

                Music.Eb ->
                    "Eb"

                Music.E ->
                    "E"

                Music.F ->
                    "F"

                Music.Fs ->
                    "Gb"

                Music.G ->
                    "G"

                Music.Ab ->
                    "Ab"

                Music.A ->
                    "A"

                Music.Bb ->
                    "Bb"

                Music.B ->
                    "B"
    in
    displayDegree ++ displayOctave
