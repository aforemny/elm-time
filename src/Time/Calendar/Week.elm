module Time.Calendar.Week exposing
    ( DayOfWeek(..)
    , dayOfWeek
    , fromInt, toInt
    )

{-| TODO

@docs DayOfWeek
@docs dayOfWeek
@docs fromInt, toInt

-}

import Time.Calendar.Days as Days exposing (Day)


{-| TODO
-}
type DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


{-| TODO
-}
fromInt : Int -> DayOfWeek
fromInt i =
    case modBy 7 i of
        0 ->
            Sunday

        1 ->
            Monday

        2 ->
            Tuesday

        3 ->
            Wednesday

        4 ->
            Thursday

        5 ->
            Friday

        _ ->
            Saturday


{-| TODO
-}
toInt : DayOfWeek -> Int
toInt dayOfWeek_ =
    case dayOfWeek_ of
        Monday ->
            1

        Tuesday ->
            2

        Wednesday ->
            3

        Thursday ->
            4

        Friday ->
            5

        Saturday ->
            6

        Sunday ->
            7


{-| TODO
-}
dayOfWeek : Day -> DayOfWeek
dayOfWeek date =
    fromInt (Days.toInt date + 3)
