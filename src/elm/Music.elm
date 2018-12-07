module Music exposing
    ( BPM
    , HalfStep(..)
    , Milliseconds
    , Note(..)
    , Octave(..)
    , compareOffset
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


type HalfStep
    = HalfStep Int


addHalfSteps : Note -> Octave -> HalfStep -> ( Note, Octave )
addHalfSteps note octave (HalfStep halfSteps) =
    (toMidiNote note octave + halfSteps)
        |> fromMidiNote
        |> Maybe.withDefault ( note, octave )


fromMidiNote : Int -> Maybe ( Note, Octave )
fromMidiNote int =
    if int < 48 || int > 83 then
        Nothing

    else
        case ( compare int 47, compare int 59, compare int 71 ) of
            ( _, _, GT ) ->
                Just ( noteFromOffset <| int - 72, Five )

            ( _, GT, _ ) ->
                Just ( noteFromOffset <| int - 60, Four )

            ( GT, _, _ ) ->
                Just ( noteFromOffset <| int - 48, Three )

            _ ->
                Nothing


noteFromOffset : Int -> Note
noteFromOffset offset =
    case modBy 12 offset of
        0 ->
            C

        1 ->
            Cs

        2 ->
            D

        3 ->
            Eb

        4 ->
            E

        5 ->
            F

        6 ->
            Fs

        7 ->
            G

        8 ->
            Ab

        9 ->
            A

        10 ->
            Bb

        11 ->
            B

        _ ->
            C


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


compareOffset : HalfStep -> HalfStep -> Order
compareOffset (HalfStep hs1) (HalfStep hs2) =
    compare hs1 hs2


defaultTempo : BPM
defaultTempo =
    BPM 120


duration : BPM -> Milliseconds
duration (BPM beatsPerMinute) =
    Milliseconds <| 1000 * 60 / toFloat beatsPerMinute / 2


millisecondsToFloat : Milliseconds -> Float
millisecondsToFloat (Milliseconds i) =
    i
