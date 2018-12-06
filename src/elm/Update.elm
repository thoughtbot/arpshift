port module Update exposing
    ( init
    , subscriptions
    , update
    )

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


subscriptions : Model -> Sub Msg
subscriptions { tempo } =
    Time.every (Music.millisecondsToFloat <| Music.duration tempo) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            let
                newModel =
                    { model | currentTick = model.currentTick + 1 }
            in
            ( newModel
            , playChord <| Model.currentChord newModel
            )
