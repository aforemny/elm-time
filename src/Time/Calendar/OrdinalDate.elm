module Time.Calendar.OrdinalDate exposing (..)

{-| ISO 8601 Ordinal Date format
-}

import Time.Calendar.Days as Days exposing (Day)
import Time.Calendar.Private exposing (..)


{-| Convert to ISO 8601 Ordinal Date format.

First element of result is year (proleptic Gregoran calendar), second is the
day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
-}
toOrdinalDate : Day -> ( Int, Int )
toOrdinalDate date =
    let
        mjd =
            Days.toInt date

        a =
            mjd + 678575

        quadcent =
            a // 146097

        b =
            a % 146097

        cent =
            min (b // 36524) 3

        c =
            b - (cent * 36524)

        quad =
            c // 1461

        d =
            c % 1461

        y =
            min (d // 365) 3

        yd =
            (d - (y * 365) + 1)

        year =
            quadcent * 400 + cent * 100 + quad * 4 + y + 1
    in
        ( year, yd )


{-| Convert from ISO 8601 Ordinal Date format.

Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
-}
fromOrdinalDate : Int -> Int -> Day
fromOrdinalDate year day =
    let
        y =
            year - 1

        mjd =
            (clip 1
                (if isLeapYear year then
                    366
                 else
                    365
                )
                day
            )
                + (365 * y)
                + (y // 4)
                - (y // 100)
                + (y // 400)
                - 678576
    in
        Days.fromInt mjd


{-| Convert from ISO 8601 Ordinal Date format.

Invalid day numbers return 'Nothing'
-}
fromOrdinalDateValid : Int -> Int -> Maybe Day
fromOrdinalDateValid year day =
    clipValid 1
        (if isLeapYear year then
            366
         else
            365
        )
        day
        |> Maybe.map
            (\day_ ->
                let
                    y =
                        year - 1

                    mjd =
                        day_ + (365 * y) + (y // 4) - (y // 100) + (y // 400) - 678576
                in
                    Days.fromInt mjd
            )


{-| Show in ISO 8601 Ordinal Date format (yyyy-ddd)
-}
showOrdinalDate : Day -> String
showOrdinalDate date =
    let
        ( y, d ) =
            toOrdinalDate date
    in
        (show4 y) ++ "-" ++ (show3 d)


{-| Is this year a leap year according to the proleptic Gregorian calendar?
-}
isLeapYear : Int -> Bool
isLeapYear year =
    (year % 4 == 0) && ((year % 400 == 0) || not (year % 100 == 0))


{-| Get the number of the Monday-starting week in the year and the day of the
week.

The first Monday is the first day of week 1, any earlier days in the year are
week 0 (as @%W@ in 'Data.Time.Format.formatTime').

Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
-}
mondayStartWeek : Day -> ( Int, Int )
mondayStartWeek date =
    let
        yd =
            Tuple.second (toOrdinalDate date)

        d =
            (Days.toInt date) + 2

        k =
            d - yd
    in
        ( (d // 7) - (k // 7), (d % 7) + 1 )


{-| Get the number of the Sunday-starting week in the year and the day of the
week.

The first Sunday is the first day of week 1, any earlier days in the year are
week 0 (as @%U@ in 'Time.Format.formatTime').

Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
-}
sundayStartWeek : Day -> ( Int, Int )
sundayStartWeek date =
    let
        yd =
            Tuple.second (toOrdinalDate date)

        d =
            (Days.toInt date) + 3

        k =
            d - yd
    in
        ( (d // 7) - (k // 7), (d % 7) )


{-| The inverse of 'mondayStartWeek'. Get a 'Day' given the year,
the number of the Monday-starting week, and the day of the week.

The first Monday is the first day of week 1, any earlier days in the year are
week 0 (as @%W@ in 'Time.Format.formatTime').
-}
fromMondayStartWeek : Int -> Int -> Int -> Day
fromMondayStartWeek year w d =
    let
        -- first day of the year
        firstDay =
            fromOrdinalDate year 1

        -- 0-based year day of first monday of the year
        zbFirstMonday =
            (5 - Days.toInt firstDay) % 7

        -- 0-based week of year
        zbWeek =
            w - 1

        -- 0-based day of week
        zbDay =
            d - 1

        -- 0-based day in year
        zbYearDay =
            zbFirstMonday + 7 * zbWeek + zbDay
    in
        Days.addDays zbYearDay firstDay


fromMondayStartWeekValid : Int -> Int -> Int -> Maybe Day
fromMondayStartWeekValid year w d =
    clipValid 1 7 d
        |> Maybe.andThen
            (\d_ ->
                let
                    -- first day of the year
                    firstDay =
                        fromOrdinalDate year 1

                    -- 0-based week of year
                    zbFirstMonday =
                        (5 - Days.toInt firstDay) % 7

                    -- 0-based week number
                    zbWeek =
                        w - 1

                    -- 0-based day of week
                    zbDay =
                        d_ - 1

                    -- 0-based day in year
                    zbYearDay =
                        zbFirstMonday + 7 * zbWeek + zbDay
                in
                    clipValid 0
                        (if isLeapYear year then
                            365
                         else
                            364
                        )
                        zbYearDay
                        |> Maybe.map
                            (\zbYearDay_ ->
                                Days.addDays zbYearDay_ firstDay
                            )
            )


{-| The inverse of 'sundayStartWeek'. Get a 'Day' given the year and
the number of the day of a Sunday-starting week.

The first Sunday is the first day of week 1, any earlier days in the year are
week 0 (as @%U@ in 'Time.Format.formatTime').
-}
fromSundayStartWeek : Int -> Int -> Int -> Day
fromSundayStartWeek year w d =
    let
        -- first day of the year
        firstDay =
            fromOrdinalDate year 1

        -- 0-based year day of first monday of the year
        zbFirstSunday =
            (4 - Days.toInt firstDay) % 7

        -- 0-based week of year
        zbWeek =
            w - 1

        -- 0-based day of week
        zbDay =
            d

        -- 0-based day in year
        zbYearDay =
            zbFirstSunday + 7 * zbWeek + zbDay
    in
        Days.addDays zbYearDay firstDay


fromSundayStartWeekValid : Int -> Int -> Int -> Maybe Day
fromSundayStartWeekValid year w d =
    clipValid 0 6 d
        |> Maybe.andThen
            (\d_ ->
                let
                    -- first day of the year
                    firstDay =
                        fromOrdinalDate year 1

                    -- 0-based week of year
                    zbFirstSunday =
                        (4 - Days.toInt firstDay) % 7

                    -- 0-based week number
                    zbWeek =
                        w - 1

                    -- 0-based day of week
                    zbDay =
                        d_

                    -- 0-based day in year
                    zbYearDay =
                        zbFirstSunday + 7 * zbWeek + zbDay
                in
                    clipValid 0
                        (if isLeapYear year then
                            365
                         else
                            364
                        )
                        zbYearDay
                        |> Maybe.map
                            (\zbYearDay_ ->
                                Days.addDays zbYearDay_ firstDay
                            )
            )
