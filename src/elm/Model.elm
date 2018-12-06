module Model exposing
    ( Chord
    , Flags
    , Model
    , Msg(..)
    , currentChord
    , initial
    )

import Music exposing (BPM, defaultTempo)


type alias Chord =
    List Int


type alias Model =
    { tempo : BPM
    , currentTick : Int
    }


type Msg
    = NoOp
    | Tick


type alias Flags =
    ()


initial : Model
initial =
    { tempo = defaultTempo
    , currentTick = 0
    }


currentChord : Model -> Chord
currentChord { currentTick } =
    if Debug.log "modBy outcome" modBy 16 currentTick > 7 then
        [ 60, 67, 69 ]

    else
        [ 60, 69, 72 ]


type SelectList a
    = SelectList (List a) a (List a)


type Lane
    = Lane
        { pitch : Int
        , notes : SelectList Bool
        }
