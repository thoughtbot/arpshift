module Music exposing
    ( BPM
    , Milliseconds
    , Note(..)
    , Octave(..)
    , defaultTempo
    , duration
    , millisecondsToFloat
    , toMidiNote
    )


type BPM
    = BPM Int


type Milliseconds
    = Milliseconds Float


type Octave
    = Three
    | Four
    | Five


type Note
    = A
    | Bb
    | B
    | C
    | Cs
    | D
    | Eb
    | E
    | F
    | Fs
    | G
    | Ab


octaveOffset : Octave -> Int
octaveOffset octave =
    case octave of
        Three ->
            48

        Four ->
            60

        Five ->
            72


midiOffset : Note -> Int
midiOffset note =
    case note of
        C ->
            0

        Cs ->
            1

        D ->
            2

        Eb ->
            3

        E ->
            4

        F ->
            5

        Fs ->
            6

        G ->
            7

        Ab ->
            8

        A ->
            9

        Bb ->
            10

        B ->
            11


toMidiNote : Note -> Octave -> Int
toMidiNote note octave =
    midiOffset note + octaveOffset octave


defaultTempo : BPM
defaultTempo =
    BPM 120


duration : BPM -> Milliseconds
duration (BPM beatsPerMinute) =
    Milliseconds <| 1000 * 60 / toFloat beatsPerMinute / 2


millisecondsToFloat : Milliseconds -> Float
millisecondsToFloat (Milliseconds i) =
    i
