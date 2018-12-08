port module Update exposing
    ( init
    , subscriptions
    , update
    )

import Lane
import List.Extra as List
import Model exposing (Chord, Model, Msg(..))
import Music
import Time


port playNote : Int -> Cmd a


init : Model.Flags -> ( Model, Cmd Msg )
init _ =
    ( Model.initial
    , Cmd.none
    )


playChord : Chord -> Cmd Msg
playChord =
    Cmd.batch << List.map playNote

playSilentNote : Cmd Msg
playSilentNote =
    playChord [0]

subscriptions : Model -> Sub Msg
subscriptions ({ tempo } as model) =
    if Model.shouldPlay model then
        Time.every (Music.millisecondsToFloat <| Music.duration tempo) (always Tick)

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            let
                newModel =
                    { model | currentTick = model.currentTick + 1, lanes = List.map Lane.tickLane model.lanes }
            in
            ( newModel
            , playChord <| Model.currentChord newModel
            )

        TogglePlay ->
            ( Model.togglePlay model, playSilentNote )

        ToggleNoteInLane lane position ->
            let
                newLanes =
                    List.updateIf (\existingLane -> lane == existingLane) (Lane.toggle position) model.lanes
            in
            ( { model | lanes = newLanes }, Cmd.none )

        ToggleLoopBack lane position ->
            let
                newLanes =
                    List.updateIf (\existingLane -> lane == existingLane) (Lane.setLoop position) model.lanes
            in
            ( { model | lanes = newLanes }, Cmd.none )

        SetOffsetOnLane lane offset ->
            let
                newLanes =
                    List.updateIf (\existingLane -> lane == existingLane) (Lane.setOffset offset) model.lanes
            in
            ( { model | lanes = newLanes }, Cmd.none )

        SetBPM newBPM ->
            ( { model | tempo = newBPM }, Cmd.none )
