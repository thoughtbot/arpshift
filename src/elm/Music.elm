module Music exposing
    ( BPM(..)
    , Degree(..)
    , HalfStep(..)
    , Milliseconds
    , Note
    , Octave(..)
    , addHalfSteps
    , availableTempos
    , compareOffset
    , defaultTempo
    , duration
    , equalsBPM
    , millisecondsToFloat
    , toMidiNote
    )


type BPM
    = BPM Int


type Milliseconds
    = Milliseconds Float


type alias Note =
    ( Degree, Octave )


type Octave
    = Three
    | Four
    | Five
    | Six


type Degree
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


addHalfSteps : Note -> HalfStep -> Note
addHalfSteps note (HalfStep halfSteps) =
    (toMidiNote note + halfSteps)
        |> fromMidiNote
        |> Maybe.withDefault note


fromMidiNote : Int -> Maybe Note
fromMidiNote int =
    let
        thirdOctaveRange =
            List.range 48 59

        fourthOctaveRange =
            List.range 60 71

        fifthOctaveRange =
            List.range 72 83

        sixthOctaveRange =
            List.range 84 95
    in
    if List.member int thirdOctaveRange then
        Just ( noteFromOffset int, Three )

    else if List.member int fourthOctaveRange then
        Just ( noteFromOffset int, Four )

    else if List.member int fifthOctaveRange then
        Just ( noteFromOffset int, Five )

    else if List.member int sixthOctaveRange then
        Just ( noteFromOffset int, Six )

    else
        Nothing


noteFromOffset : Int -> Degree
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

        Six ->
            84


midiOffset : Degree -> Int
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


toMidiNote : Note -> Int
toMidiNote ( note, octave ) =
    midiOffset note + octaveOffset octave


compareOffset : HalfStep -> HalfStep -> Order
compareOffset (HalfStep hs1) (HalfStep hs2) =
    compare hs1 hs2


defaultTempo : BPM
defaultTempo =
    BPM 120


availableTempos : List BPM
availableTempos =
    [ BPM 98, BPM 120, BPM 144 ]


equalsBPM : BPM -> BPM -> Bool
equalsBPM (BPM bpm1) (BPM bpm2) =
    bpm1 == bpm2


duration : BPM -> Milliseconds
duration (BPM beatsPerMinute) =
    Milliseconds <| 1000 * 60 / toFloat beatsPerMinute / 2


millisecondsToFloat : Milliseconds -> Float
millisecondsToFloat (Milliseconds i) =
    i
