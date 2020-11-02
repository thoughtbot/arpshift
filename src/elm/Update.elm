port module Update exposing
    ( init
    , subscriptions
    , update
    )

import Lane
import List.Extra as List
import Model exposing (Chord, Model, Msg(..))
import Music
import Music.Random as Music
import Random
import Random.Extra as Random
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
    playChord [ 0 ]


subscriptions : Model -> Sub Msg
subscriptions ({ tempo } as model) =
    if Model.shouldPlay model then
        Time.every (Music.millisecondsToFloat <| Music.duration tempo) (always Tick)

    else
        Sub.none


generateRandomNotes : Model -> Cmd Msg
generateRandomNotes model =
    let
        bassProbability =
            Music.generateProbability 1 3 |> Maybe.withDefault Music.alwaysHappens

        midProbability =
            Music.generateProbability 1 4 |> Maybe.withDefault Music.alwaysHappens

        highProbability =
            Music.generateProbability 1 6 |> Maybe.withDefault Music.alwaysHappens
    in
    [ Music.lowOctave
        |> Music.noteWithinMode model.mode model.degree
        |> Music.withProbability bassProbability
    , Music.midOctave
        |> Music.noteWithinMode model.mode model.degree
        |> Music.withProbability midProbability
    , Music.midOctave
        |> Music.noteWithinMode model.mode model.degree
        |> Music.withProbability highProbability
    , Music.highOctave
        |> Music.noteWithinMode model.mode model.degree
        |> Music.withProbability highProbability
    ]
        |> Random.combine
        |> Random.generate SetRandomNotes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateLane lane f =
            List.updateIf ((==) lane) f model.lanes
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetRandomNote note ->
            ( { model | randomNotes = [ note ] }
            , Cmd.none
            )

        SetRandomNotes notes ->
            ( { model | randomNotes = notes }
            , Cmd.none
            )

        Tick ->
            let
                newModel =
                    { model | currentTick = model.currentTick + 1, lanes = List.map Lane.tickLane model.lanes }
            in
            ( newModel
            , Cmd.batch
                [ playChord <| Model.currentChord newModel
                ]
            )

        TogglePlay ->
            ( Model.togglePlay model, playSilentNote )

        ToggleNoteInLane lane position ->
            let
                newLanes =
                    updateLane lane (Lane.toggle position)
            in
            ( { model | lanes = newLanes }, Cmd.none )

        ToggleLoopBack lane position ->
            let
                newLanes =
                    updateLane lane (Lane.setLoop position)
            in
            ( { model | lanes = newLanes }, Cmd.none )

        SetOffsetOnLane lane offset ->
            let
                newLanes =
                    updateLane lane (Lane.setOffset offset)
            in
            ( { model | lanes = newLanes }, Cmd.none )

        SetBPM newBPM ->
            ( { model | tempo = newBPM }, Cmd.none )

        SetDegree newDegree ->
            ( { model
                | degree = newDegree
                , lanes = Model.buildLanes Lane.setScaleAndNote model.mode newDegree model.lanes
              }
            , Cmd.none
            )

        SetMode newMode ->
            ( { model
                | mode = newMode
                , lanes = Model.buildLanes Lane.setScaleAndNote newMode model.degree model.lanes
              }
            , Cmd.none
            )
