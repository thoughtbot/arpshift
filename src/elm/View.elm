module View exposing (view)

import Degree
import Html exposing (..)
import Html.Attributes
    exposing
        ( checked
        , class
        , classList
        , href
        , name
        , selected
        , title
        , type_
        , value
        )
import Html.Events exposing (on, onClick, targetValue)
import Html.Keyed
import Json.Decode
import Lane exposing (Lane)
import Mode
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
            , div [ class "nav-right" ]
                [ div [ class "bpm-label" ]
                    [ select
                        [ on "change" (Json.Decode.map SetBPM bpmDecoder) ]
                        (List.map generateTempoOption Music.availableTempos)
                    ]
                , div [ class "degree-label" ]
                    [ select
                        [ on "change" (Json.Decode.map SetDegree degreeDecoder) ]
                        (List.map generateDegreeOption Degree.all)
                    ]
                , div [ class "mode-label" ]
                    [ select
                        [ on "change" (Json.Decode.map SetMode modeDecoder) ]
                        (List.map generateModeOption Mode.all)
                    ]
                , playPauseButton model
                ]
            ]
        , main_ [ class "main" ] <| (List.map viewLane <| withIndex model.lanes)
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


generateTempoOption : Music.BPM -> Html a
generateTempoOption ((Music.BPM bpm) as fullBPM) =
    option
        [ value <| String.fromInt bpm
        , selected <| Music.equalsBPM Music.defaultTempo fullBPM
        ]
        [ text <| String.fromInt bpm ]


generateDegreeOption : Degree.Degree -> Html a
generateDegreeOption degree =
    option
        [ value <| Degree.toString degree
        ]
        [ text <| Degree.toString degree ]


generateModeOption : Mode.Mode -> Html a
generateModeOption degree =
    option
        [ value <| Mode.toString degree
        ]
        [ text <| Mode.toString degree ]


bpmDecoder : Json.Decode.Decoder Music.BPM
bpmDecoder =
    targetValue
        |> Json.Decode.andThen
            (\v ->
                let
                    mBPM =
                        String.toInt v |> Maybe.map Music.BPM
                in
                case mBPM of
                    Nothing ->
                        Json.Decode.fail "not a number"

                    Just bpm ->
                        Json.Decode.succeed bpm
            )


degreeDecoder : Json.Decode.Decoder Degree.Degree
degreeDecoder =
    targetValue
        |> Json.Decode.andThen
            (\v ->
                case Degree.fromString v of
                    Just d ->
                        Json.Decode.succeed d

                    Nothing ->
                        Json.Decode.fail "Not a degree"
            )


modeDecoder : Json.Decode.Decoder Mode.Mode
modeDecoder =
    targetValue
        |> Json.Decode.andThen
            (\v ->
                case Mode.fromString v of
                    Just d ->
                        Json.Decode.succeed d

                    Nothing ->
                        Json.Decode.fail "Not a mode"
            )


viewLane : ( Lane, Int ) -> Html Msg
viewLane ( lane, laneIndex ) =
    let
        pitchNode v =
            let
                isActive =
                    Music.compareOffset (Lane.laneOffset lane) v == EQ

                currentClass =
                    case Music.compareOffset (Lane.laneOffset lane) v of
                        GT ->
                            "pitch-node-on"

                        EQ ->
                            "pitch-node-active"

                        LT ->
                            "pitch-node-off"

                appliedNote =
                    Music.addHalfSteps (Lane.laneNote lane) v

                spanBody =
                    if isActive then
                        [ span [ class "active-node-text" ] [ text <| noteToString appliedNote ] ]

                    else
                        []
            in
            span
                [ class currentClass
                , classList [ ( "hide", not <| Music.withinScale appliedNote (Lane.laneScale lane) ) ]
                , title <| noteToString appliedNote
                , onClick <| SetOffsetOnLane lane v
                ]
                spanBody

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
        [ div [ class "pitch" ] (List.map pitchNode halfStepValues)
        , div [ class "notes" ] (List.map viewNote <| withIndex notes)
        ]


noteToString : Music.Note -> String
noteToString ( degree, octave ) =
    let
        displayOctave =
            case octave of
                Music.Two ->
                    "2"

                Music.Three ->
                    "3"

                Music.Four ->
                    "4"

                Music.Five ->
                    "5"

                Music.Six ->
                    "6"

        displayDegree =
            Degree.toString degree
    in
    displayDegree ++ displayOctave
