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
    { tempo = defaultTempo
    , currentTick = 0
    , lanes =
        [ Lane.initial ( Music.D, Music.Five )
        , Lane.initial ( Music.B, Music.Four )
        , Lane.initial ( Music.G, Music.Four )
        , Lane.initial ( Music.D, Music.Four )
        , Lane.initial ( Music.G, Music.Three )
        , Lane.initial ( Music.D, Music.Three )
        ]
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
