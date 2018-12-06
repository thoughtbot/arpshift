module Model exposing
    ( Chord
    , Flags
    , Model
    , Msg(..)
    , currentChord
    , initial
    , tickLane
    )

import Music exposing (BPM, defaultTempo)
import SelectList exposing (SelectList(..))


type alias Chord =
    List Int


type alias Model =
    { tempo : BPM
    , currentTick : Int
    , lanes : List Lane
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
    , lanes =
        [ initialLane (NoteNumber 60)
            |> set 1 True
        , initialLane (NoteNumber 67)
            |> set 2 True
            |> set 5 True
            |> set 0 False
        ]
    }


set : Int -> Bool -> Lane -> Lane
set position newValue (Lane lane) =
    let
        newNotes =
            SelectList.updateAt position (always newValue) lane.notes
    in
    Lane { lane | notes = newNotes }


currentChord : Model -> Chord
currentChord { lanes } =
    let
        catMaybes =
            List.filterMap identity
    in
    List.map currentNoteForLane lanes
        |> catMaybes
        |> List.map (\(NoteNumber v) -> v)


type NoteNumber
    = NoteNumber Int


type Lane
    = Lane
        { pitch : NoteNumber
        , notes : SelectList Bool
        , loopAt : Int
        }


initialLane : NoteNumber -> Lane
initialLane noteNumber =
    Lane
        { pitch = noteNumber
        , notes = SelectList [] True [ False, False, True, True, False, False, False ]
        , loopAt = 8
        }


tickLane : Lane -> Lane
tickLane (Lane lane) =
    let
        (SelectList before selected after) =
            lane.notes

        newNotes =
            case ( after, List.length before + 1 == lane.loopAt ) of
                ( [], _ ) ->
                    SelectList.moveToBeginningOfSelectList lane.notes

                ( _, True ) ->
                    SelectList.moveToBeginningOfSelectList lane.notes

                ( x :: xs, _ ) ->
                    SelectList (before ++ [ selected ]) x xs
    in
    Lane { lane | notes = newNotes }


currentNoteForLane : Lane -> Maybe NoteNumber
currentNoteForLane (Lane { notes, pitch }) =
    if SelectList.active notes then
        Just pitch

    else
        Nothing
