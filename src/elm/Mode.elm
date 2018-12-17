module Mode exposing
    ( Mode(..)
    , all
    , fromString
    , toString
    )


type Mode
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian


all : List Mode
all =
    [ Ionian
    , Dorian
    , Phrygian
    , Lydian
    , Mixolydian
    , Aeolian
    , Locrian
    ]


toString : Mode -> String
toString mode =
    case mode of
        Ionian ->
            "Ionian"

        Dorian ->
            "Dorian"

        Phrygian ->
            "Phrygian"

        Lydian ->
            "Lydian"

        Mixolydian ->
            "Mixolydian"

        Aeolian ->
            "Aeolian"

        Locrian ->
            "Locrian"


fromString : String -> Maybe Mode
fromString value =
    case value of
        "Ionian" ->
            Just Ionian

        "Dorian" ->
            Just Dorian

        "Phrygian" ->
            Just Phrygian

        "Lydian" ->
            Just Lydian

        "Mixolydian" ->
            Just Mixolydian

        "Aeolian" ->
            Just Aeolian

        "Locrian" ->
            Just Locrian

        _ ->
            Nothing
