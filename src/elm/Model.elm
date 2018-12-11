module Model exposing
    ( Chord
    , Flags
    , Model
    , Msg(..)
    , currentChord
    , initial
    , shouldPlay
    , togglePlay
    )

import Lane exposing (Lane)
import Music exposing (BPM, HalfStep, defaultTempo)


type alias Chord =
    List Int


type PlayState
    = Playing
    | Paused


type alias Model =
    { tempo : BPM
    , currentTick : Int
    , lanes : List Lane
    , playState : PlayState
    }


type Msg
    = NoOp
    | Tick
    | TogglePlay
    | ToggleNoteInLane Lane Int
    | ToggleLoopBack Lane Int
    | SetOffsetOnLane Lane HalfStep
    | SetBPM BPM


type alias Flags =
    ()


initial : Model
initial =
    let
        root =
            Music.E

        mode =
            Music.Aeolian

        calculatedRoots =
            Music.roots 6 ( root, Music.Three ) mode
    in
    { tempo = defaultTempo
    , currentTick = 0
    , lanes =
        List.map (Lane.initial <| Music.transposeScale ( root, Music.Three ) mode) calculatedRoots
    , playState = Paused
    }


currentChord : Model -> Chord
currentChord { lanes } =
    let
        catMaybes =
            List.filterMap identity
    in
    List.map Lane.currentNoteForLane lanes
        |> catMaybes
        |> List.map Music.toMidiNote


togglePlay : Model -> Model
togglePlay ({ playState } as model) =
    case playState of
        Playing ->
            { model | playState = Paused }

        Paused ->
            { model | playState = Playing }


shouldPlay : Model -> Bool
shouldPlay { playState } =
    playState == Playing
