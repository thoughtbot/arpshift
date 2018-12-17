module Model exposing
    ( Chord
    , Flags
    , Model
    , Msg(..)
    , buildLanes
    , currentChord
    , initial
    , shouldPlay
    , togglePlay
    )

import Degree
import Lane exposing (Lane)
import Mode
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
    , degree : Degree.Degree
    , mode : Mode.Mode
    }


type Msg
    = NoOp
    | Tick
    | TogglePlay
    | ToggleNoteInLane Lane Int
    | ToggleLoopBack Lane Int
    | SetOffsetOnLane Lane HalfStep
    | SetBPM BPM
    | SetDegree Degree.Degree
    | SetMode Mode.Mode


type alias Flags =
    ()


initial : Model
initial =
    let
        root =
            Degree.C

        mode =
            Mode.Ionian
    in
    { tempo = defaultTempo
    , currentTick = 0
    , lanes =
        List.repeat 6 Lane.initial_
            |> buildLanes Lane.setScaleAndNote mode root
    , playState = Paused
    , degree = root
    , mode = mode
    }


buildLanes : (List Music.Note -> Music.Note -> Lane -> Lane) -> Mode.Mode -> Degree.Degree -> List Lane.Lane -> List Lane.Lane
buildLanes f mode root lanes =
    let
        calculatedRoots =
            Music.roots ( root, Music.Four ) mode
    in
    List.map2 Tuple.pair calculatedRoots lanes
        |> List.map
            (\( r, l ) ->
                f (Music.transposeScale ( root, Music.Three ) mode) r l
            )


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
