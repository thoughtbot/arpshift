module SelectList exposing
    ( Position(..)
    , SelectList(..)
    , active
    , moveToBeginningOfSelectList
    , toList
    , toListWithPosition
    , updateAt
    , valueAt
    )

import List.Extra as List


type SelectList a
    = SelectList (List a) a (List a)


toList : SelectList a -> List a
toList =
    List.map Tuple.first << toListWithPosition


moveToBeginningOfSelectList : SelectList a -> SelectList a
moveToBeginningOfSelectList ((SelectList before selected after) as selectList) =
    let
        fullList =
            toList selectList

        ( x, xs ) =
            case ( List.head fullList, List.tail fullList ) of
                ( Just v, Just vs ) ->
                    ( v, vs )

                ( Just v, Nothing ) ->
                    ( v, [] )

                _ ->
                    ( selected, [] )
    in
    SelectList [] x xs


active : SelectList a -> a
active (SelectList _ v _) =
    v


length : SelectList a -> Int
length =
    List.length << toList


type Position
    = Before
    | Selected
    | After


toListWithPosition : SelectList a -> List ( a, Position )
toListWithPosition (SelectList before selected after) =
    List.map (\v -> ( v, Before )) before
        ++ [ ( selected, Selected ) ]
        ++ List.map (\v -> ( v, After )) after


fromListWithPosition : a -> List ( a, Position ) -> SelectList a
fromListWithPosition default xs =
    let
        valuesByPosition position =
            List.filter (\( _, pos ) -> pos == position) xs |> List.map extractValue

        extractValue ( v, _ ) =
            v

        before =
            valuesByPosition Before

        selected =
            valuesByPosition Selected |> List.head |> Maybe.withDefault default

        after =
            valuesByPosition After
    in
    SelectList before selected after


valueAt : Int -> SelectList a -> Maybe a
valueAt position ((SelectList before selected after) as list) =
    toList list
        |> List.getAt position


updateAt : Int -> (a -> a) -> SelectList a -> SelectList a
updateAt position f ((SelectList before selected after) as list) =
    let
        withinBounds =
            position >= 0 && position < length list
    in
    if withinBounds then
        list
            |> toListWithPosition
            |> List.updateAt position (Tuple.mapFirst f)
            |> fromListWithPosition selected

    else
        list
