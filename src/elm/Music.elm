module Music exposing
    ( BPM(..)
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
    , roots
    , toMidiNote
    , transposeScale
    , withinScale
    )

import Degree exposing (Degree)
import Mode exposing (Mode)


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


type HalfStep
    = HalfStep Int


modeScaleDegrees : Mode.Mode -> List HalfStep
modeScaleDegrees mode =
    let
        hs =
            HalfStep
    in
    case mode of
        Mode.Ionian ->
            [ hs 0, hs 2, hs 4, hs 5, hs 7, hs 9, hs 11, hs 12 ]

        Mode.Dorian ->
            [ hs 0, hs 2, hs 3, hs 5, hs 7, hs 9, hs 10, hs 12 ]

        Mode.Phrygian ->
            [ hs 0, hs 1, hs 3, hs 5, hs 7, hs 8, hs 10, hs 12 ]

        Mode.Lydian ->
            [ hs 0, hs 2, hs 4, hs 6, hs 7, hs 9, hs 11, hs 12 ]

        Mode.Mixolydian ->
            [ hs 0, hs 2, hs 4, hs 5, hs 7, hs 9, hs 10, hs 12 ]

        Mode.Aeolian ->
            [ hs 0, hs 2, hs 3, hs 5, hs 7, hs 8, hs 10, hs 12 ]

        Mode.Locrian ->
            [ hs 0, hs 1, hs 3, hs 5, hs 6, hs 8, hs 10, hs 12 ]


addHalfSteps : Note -> HalfStep -> Note
addHalfSteps note (HalfStep halfSteps) =
    (toMidiNote note + halfSteps)
        |> fromMidiNote
        |> Maybe.withDefault note


withinScale : Note -> List Note -> Bool
withinScale ( degree, _ ) notes =
    let
        noteDegree ( deg, _ ) =
            deg
    in
    List.map noteDegree notes
        |> List.member degree


roots : Note -> Mode -> List Note
roots note mode =
    let
        majorOpen =
            [ addHalfSteps note (HalfStep 24)
            , addHalfSteps note (HalfStep 16)
            , addHalfSteps note (HalfStep 12)
            , addHalfSteps note (HalfStep 7)
            , note
            , addHalfSteps note (HalfStep -5)
            ]

        minorOpen =
            [ addHalfSteps note (HalfStep 24)
            , addHalfSteps note (HalfStep 15)
            , addHalfSteps note (HalfStep 12)
            , addHalfSteps note (HalfStep 7)
            , note
            , addHalfSteps note (HalfStep -5)
            ]

        minorDim5Open =
            [ addHalfSteps note (HalfStep 24)
            , addHalfSteps note (HalfStep 15)
            , addHalfSteps note (HalfStep 12)
            , addHalfSteps note (HalfStep 6)
            , note
            , addHalfSteps note (HalfStep -6)
            ]
    in
    case mode of
        Mode.Ionian ->
            majorOpen

        Mode.Lydian ->
            majorOpen

        Mode.Mixolydian ->
            majorOpen

        Mode.Phrygian ->
            minorOpen

        Mode.Aeolian ->
            minorOpen

        Mode.Dorian ->
            minorOpen

        Mode.Locrian ->
            minorDim5Open


transposeScale : Note -> Mode -> List Note
transposeScale note mode =
    modeScaleDegrees mode
        |> List.map (addHalfSteps note)


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
        Just ( Degree.fromOffset int, Three )

    else if List.member int fourthOctaveRange then
        Just ( Degree.fromOffset int, Four )

    else if List.member int fifthOctaveRange then
        Just ( Degree.fromOffset int, Five )

    else if List.member int sixthOctaveRange then
        Just ( Degree.fromOffset int, Six )

    else
        Nothing


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


toMidiNote : Note -> Int
toMidiNote ( note, octave ) =
    Degree.toOffset note + octaveOffset octave


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
