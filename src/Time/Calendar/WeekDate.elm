module Time.Calendar.WeekDate exposing (..)

{-| ISO 8601 Week Date format
-}

import Time.Calendar.OrdinalDate exposing (..)
import Time.Calendar.Days as Days exposing (..)
import Time.Calendar.Private exposing (..)


{-| Convert to ISO 8601 Week Date format. First element of result is year,
second week number (1-53), third day of week (1 for Monday to 7 for Sunday).

Note that \"Week\" years are not quite the same as Gregorian years, as the
first day of the year is always a Monday.

The first week of a year is the first week to contain at least four days in the
corresponding Gregorian year.
-}
toWeekDate : Day -> ( Int, Int, Int )
toWeekDate date =
    let
        mjd =
            Days.toInt date

        ( d_div_7, d_mod_7 ) =
            ( d // 7, d % 7 )

        ( y0, yd ) =
            toOrdinalDate date

        d =
            mjd + 2

        foo y =
            bar (Days.toInt (fromOrdinalDate y 6))

        bar k =
            d_div_7 - k // 7

        ( y1, w1 ) =
            case bar (d - yd + 4) of
                (-1) ->
                    ( y0 - 1, foo (y0 - 1) )

                52 ->
                    if foo (y0 + 1) == 0 then
                        ( y0 + 1, 0 )
                    else
                        ( y0, 52 )

                w0 ->
                    ( y0, w0 )
    in
        ( y1, (w1 + 1), d_mod_7 + 1 )


{-| Convert from ISO 8601 Week Date format. First argument is year, second week
number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).

Invalid week and day values will be clipped to the correct range.
-}
fromWeekDate : Int -> Int -> Int -> Day
fromWeekDate y w d =
    let
        k =
            Days.toInt (fromOrdinalDate y 6)

        longYear =
            case toWeekDate (fromOrdinalDate y 365) of
                ( _, 53, _ ) ->
                    True

                _ ->
                    False
    in
        Days.fromInt
            (k - (k % 7)
                + (((clip 1
                        (if longYear then
                            53
                         else
                            52
                        )
                        w
                    )
                        * 7
                   )
                    + (clip 1 7 d)
                  )
                - 10
            )


{-| Convert from ISO 8601 Week Date format. First argument is year, second week
number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).

Invalid week and day values will return Nothing.
-}
fromWeekDateValid : Int -> Int -> Int -> Maybe Day
fromWeekDateValid y w d =
    clipValid 1 7 d
        |> Maybe.andThen
            (\d_ ->
                let
                    longYear =
                        case toWeekDate (fromOrdinalDate y 365) of
                            ( _, 53, _ ) ->
                                True

                            _ ->
                                False
                in
                    clipValid 1
                        (if longYear then
                            53
                         else
                            52
                        )
                        w
                        |> Maybe.map
                            (\w_ ->
                                let
                                    k =
                                        Days.toInt (fromOrdinalDate y 6)
                                in
                                    Days.fromInt (k - (k % 7) + (((w_ * 7) + d_)) - 10)
                            )
            )


{-| Show in ISO 8601 Week Date format as yyyy-Www-d (e.g. \"2006-W46-3\").
-}
showWeekDate : Day -> String
showWeekDate date =
    let
        ( y, w, d ) =
            toWeekDate date
    in
        (show4 y) ++ "-W" ++ (show2 w) ++ "-" ++ (toString d)
