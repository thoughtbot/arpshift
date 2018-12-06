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
import Music exposing (BPM, defaultTempo)


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


type alias Flags =
    ()


initial : Model
initial =
    { tempo = defaultTempo
    , currentTick = 0
    , lanes =
        [ Lane.initial 30
            |> Lane.setLoop 2
            |> Lane.turnOn 0
        , Lane.initial 60
            |> Lane.turnOn 1
            |> Lane.setLoop 4
        , Lane.initial 67
            |> Lane.turnOn 2
            |> Lane.turnOn 5
            |> Lane.setLoop 5
        , Lane.initial 69
            |> Lane.turnOn 4
            |> Lane.setLoop 5
        ]
    , playState = Playing
    }


currentChord : Model -> Chord
currentChord { lanes } =
    let
        catMaybes =
            List.filterMap identity
    in
    List.map Lane.currentNoteForLane lanes
        |> catMaybes


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
