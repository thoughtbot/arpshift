module Lane exposing
    ( Lane
    , NoteNumber(..)
    , currentLoopPosition
    , currentNoteForLane
    , enabled
    , initial
    , laneNotes
    , laneOffset
    , setLoop
    , setOffset
    , tickLane
    , toggle
    , turnOff
    , turnOn
    )

import SelectList exposing (SelectList(..))


type Lane
    = Lane
        { pitch : NoteNumber
        , notes : SelectList Bool
        , loopAt : Int
        , offset : Int
        }


type NoteNumber
    = NoteNumber Int


laneNotes : Lane -> SelectList Bool
laneNotes (Lane lane) =
    lane.notes


laneOffset : Lane -> Int
laneOffset (Lane { offset }) =
    offset


initial : Int -> Lane
initial noteNumber =
    Lane
        { pitch = NoteNumber noteNumber
        , notes = SelectList [] False (List.repeat 7 False)
        , loopAt = 7
        , offset = 0
        }


setLoop : Int -> Lane -> Lane
setLoop i (Lane lane) =
    Lane { lane | loopAt = i }


setOffset : Int -> Lane -> Lane
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


currentNoteForLane : Lane -> Maybe Int
currentNoteForLane (Lane { notes, pitch, offset }) =
    if SelectList.active notes then
        let
            (NoteNumber rawPitch) =
                pitch
        in
        Just <| rawPitch + offset

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
