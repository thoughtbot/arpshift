module Lane exposing
    ( Lane
    , currentLoopPosition
    , currentNoteForLane
    , enabled
    , initial
    , laneNote
    , laneNotes
    , laneOffset
    , laneTransposedNote
    , setLoop
    , setOffset
    , tickLane
    , toggle
    , turnOff
    , turnOn
    )

import Music exposing (HalfStep(..))
import SelectList exposing (SelectList(..))


type Lane
    = Lane
        { note : Music.Note
        , notes : SelectList Bool
        , loopAt : Int
        , offset : HalfStep
        }


laneNotes : Lane -> SelectList Bool
laneNotes (Lane lane) =
    lane.notes


laneNote : Lane -> Music.Note
laneNote (Lane lane) =
    lane.note


laneTransposedNote : Lane -> Music.Note
laneTransposedNote (Lane lane) =
    Music.addHalfSteps lane.note lane.offset


laneOffset : Lane -> HalfStep
laneOffset (Lane { offset }) =
    offset


initial : Music.Note -> Lane
initial note =
    Lane
        { note = note
        , notes = SelectList [] False (List.repeat 7 False)
        , loopAt = 7
        , offset = HalfStep 0
        }


setLoop : Int -> Lane -> Lane
setLoop i (Lane lane) =
    Lane { lane | loopAt = i }


setOffset : HalfStep -> Lane -> Lane
setOffset i (Lane lane) =
    Lane { lane | offset = i }


tickLane : Lane -> Lane
tickLane (Lane lane) =
    let
        (SelectList before selected after) =
            lane.notes

        newNotes =
            case ( after, List.length before == lane.loopAt ) of
                ( [], _ ) ->
                    SelectList.moveToBeginningOfSelectList lane.notes

                ( _, True ) ->
                    SelectList.moveToBeginningOfSelectList lane.notes

                ( x :: xs, _ ) ->
                    SelectList (before ++ [ selected ]) x xs
    in
    Lane { lane | notes = newNotes }


currentNoteForLane : Lane -> Maybe Music.Note
currentNoteForLane (Lane { notes, note, offset }) =
    if SelectList.active notes then
        Just <| Music.addHalfSteps note offset

    else
        Nothing


turnOn : Int -> Lane -> Lane
turnOn position lane =
    set position True lane


turnOff : Int -> Lane -> Lane
turnOff position lane =
    set position False lane


toggle : Int -> Lane -> Lane
toggle position (Lane lane) =
    let
        currentValue =
            SelectList.valueAt position lane.notes
    in
    case currentValue of
        Nothing ->
            Lane lane

        Just foundValue ->
            set position (not foundValue) (Lane lane)


set : Int -> Bool -> Lane -> Lane
set position newValue (Lane lane) =
    let
        newNotes =
            SelectList.updateAt position (always newValue) lane.notes
    in
    Lane { lane | notes = newNotes }


enabled : Int -> Lane -> Bool
enabled position (Lane { loopAt }) =
    position <= loopAt


currentLoopPosition : Int -> Lane -> Bool
currentLoopPosition position (Lane { loopAt }) =
    position == loopAt
