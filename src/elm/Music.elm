module Music exposing
    ( BPM
    , Milliseconds
    , defaultTempo
    , duration
    , millisecondsToFloat
    )


type BPM
    = BPM Int


type Milliseconds
    = Milliseconds Float


defaultTempo : BPM
defaultTempo =
    BPM 120


duration : BPM -> Milliseconds
duration (BPM beatsPerMinute) =
    Milliseconds <| 1000 * 60 / toFloat beatsPerMinute / 2


millisecondsToFloat : Milliseconds -> Float
millisecondsToFloat (Milliseconds i) =
    i
