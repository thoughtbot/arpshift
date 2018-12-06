module Lane exposing
    ( Lane
    , currentNoteForLane
    , initial
    , setLoop
    , tickLane
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


initial : Int -> Lane
initial noteNumber =
    Lane
        { pitch = NoteNumber noteNumber
        , notes = SelectList [] False (List.repeat 7 False)
        , loopAt = 8
        , offset = 0
        }


setLoop : Int -> Lane -> Lane
setLoop i (Lane lane) =
    Lane { lane | loopAt = i }


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


set : Int -> Bool -> Lane -> Lane
set position newValue (Lane lane) =
    let
        newNotes =
            SelectList.updateAt position (always newValue) lane.notes
    in
    Lane { lane | notes = newNotes }
