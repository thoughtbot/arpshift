module Degree exposing
    ( Degree(..)
    , all
    , fromOffset
    , fromString
    , toOffset
    , toString
    )


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


all : List Degree
all =
    [ C, Cs, D, Eb, E, F, Fs, G, Ab, A, Bb, B ]


fromString : String -> Maybe Degree
fromString value =
    case value of
        "A" ->
            Just A

        "B" ->
            Just B

        "Bb" ->
            Just Bb

        "C" ->
            Just C

        "C#" ->
            Just Cs

        "D" ->
            Just D

        "Eb" ->
            Just Eb

        "E" ->
            Just E

        "F" ->
            Just F

        "F#" ->
            Just Fs

        "G" ->
            Just G

        "Ab" ->
            Just Ab

        _ ->
            Nothing


toString : Degree -> String
toString degree =
    case degree of
        A ->
            "A"

        Bb ->
            "Bb"

        B ->
            "B"

        C ->
            "C"

        Cs ->
            "C#"

        D ->
            "D"

        Eb ->
            "Eb"

        E ->
            "E"

        F ->
            "F"

        Fs ->
            "F#"

        G ->
            "G"

        Ab ->
            "Ab"


fromOffset : Int -> Degree
fromOffset offset =
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


toOffset : Degree -> Int
toOffset note =
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
