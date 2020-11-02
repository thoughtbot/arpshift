module Music.Random exposing
    ( alwaysHappens
    , degree
    , generateProbability
    , highOctave
    , lowOctave
    , midOctave
    , noteWithinMode
    , withProbability
    )

import Degree
import Mode
import Music
import Random


type Probability
    = Probability Int Int


generateProbability : Int -> Int -> Maybe Probability
generateProbability numerator denominator =
    if numerator > 0 && denominator >= numerator then
        Just <| Probability numerator denominator

    else
        Nothing


alwaysHappens : Probability
alwaysHappens =
    Probability 1 1


withProbability : Probability -> Random.Generator a -> Random.Generator (Maybe a)
withProbability (Probability num denom) gen =
    gen
        |> Random.andThen
            (\v ->
                Random.weighted
                    ( toFloat (denom - num), Nothing )
                    [ ( toFloat num, Just v ) ]
            )


noteWithinMode : Mode.Mode -> Degree.Degree -> Random.Generator Music.Octave -> Random.Generator Music.Note
noteWithinMode mode deg_ oct =
    oct
        |> Random.map
            (\oct_ ->
                Music.transposeScale ( deg_, oct_ ) mode
            )
        |> Random.andThen
            (\notes ->
                uniformList notes ( Degree.C, Music.Two )
            )


degree : Random.Generator Degree.Degree
degree =
    uniformList Degree.all Degree.C


highOctave : Random.Generator Music.Octave
highOctave =
    uniformList [ Music.Five ] Music.Six


midOctave : Random.Generator Music.Octave
midOctave =
    uniformList [ Music.Three ] Music.Four


lowOctave : Random.Generator Music.Octave
lowOctave =
    uniformList [ Music.Two ] Music.Three


uniformList : List a -> a -> Random.Generator a
uniformList all default =
    let
        ( first, rest ) =
            case all of
                [] ->
                    ( default, [] )

                x :: xs ->
                    ( x, xs )
    in
    Random.uniform first rest
