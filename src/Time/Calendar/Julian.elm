module Time.Calendar.Julian
    exposing
        ( toJulian
        , fromJulian
        , fromJulianValid
        , showJulian
        , julianMonthLength
        , addJulianMonthsClip
        , addJulianMonthsRollOver
        , addJulianYearsClip
        , addJulianYearsRollOver
        , addJulianDurationClip
        , addJulianDurationRollOver
        , diffJulianDurationClip
        , diffJulianDurationRollOver
        )

import Time.Calendar.CalendarDiffDays exposing (..)
import Time.Calendar.Days as Days exposing (..)
import Time.Calendar.JulianYearDay exposing (..)
import Time.Calendar.MonthDay exposing (..)
import Time.Calendar.Private exposing (..)


{-| Convert to proleptic Julian calendar. First element of result is year,
second month number (1-12), third day (1-31).
-}
toJulian : Day -> ( Int, Int, Int )
toJulian date =
    let
        ( year, yd ) =
            toJulianYearAndDay date

        ( month, day ) =
            dayOfYearToMonthAndDay (isJulianLeapYear year) yd
    in
        ( year, month, day )


{-| Convert from proleptic Julian calendar. First argument is year, second
month number (1-12), third day (1-31).

Invalid values will be clipped to the correct range, month first, then day.
-}
fromJulian : Int -> Int -> Int -> Day
fromJulian year month day =
    fromJulianYearAndDay year (monthAndDayToDayOfYear (isJulianLeapYear year) month day)


{-| Convert from proleptic Julian calendar. First argument is year, second
month number (1-12), third day (1-31).

Invalid values will return Nothing.
-}
fromJulianValid : Int -> Int -> Int -> Maybe Day
fromJulianValid year month day =
    monthAndDayToDayOfYearValid (isJulianLeapYear year) month day
        |> Maybe.andThen
            (\doy ->
                fromJulianYearAndDayValid year doy
            )


{-| Show in ISO 8601 format (yyyy-mm-dd)
-}
showJulian : Day -> String
showJulian date =
    let
        ( y, m, d ) =
            toJulian date
    in
        (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)


{-| The number of days in a given month according to the proleptic Julian
calendar. First argument is year, second is month.
-}
julianMonthLength : Int -> Int -> Int
julianMonthLength year =
    monthLength (isJulianLeapYear year)


rolloverMonths : ( Int, Int ) -> ( Int, Int )
rolloverMonths ( y, m ) =
    ( y + ((m - 1) // 12), ((m - 1) % 12) + 1 )


addJulianMonths : Int -> Day -> ( Int, Int, Int )
addJulianMonths n day =
    let
        ( y, m, d ) =
            toJulian day

        ( y_, m_ ) =
            rolloverMonths ( y, m + n )
    in
        ( y_, m_, d )


{-| Add months, with days past the last day of the month clipped to the last
day.

For instance, 2005-01-30 + 1 month = 2005-02-28.
-}
addJulianMonthsClip : Int -> Day -> Day
addJulianMonthsClip n day =
    let
        ( y, m, d ) =
            addJulianMonths n day
    in
        fromJulian y m d


{-| Add months, with days past the last day of the month rolling over to the
next month.

For instance, 2005-01-30 + 1 month = 2005-03-02.
-}
addJulianMonthsRollOver : Int -> Day -> Day
addJulianMonthsRollOver n day =
    let
        ( y, m, d ) =
            addJulianMonths n day
    in
        addDays (d - 1) (fromJulian y m 1)


{-| Add years, matching month and day, with Feb 29th clipped to Feb 28th if
necessary.

For instance, 2004-02-29 + 2 years = 2006-02-28.
-}
addJulianYearsClip : Int -> Day -> Day
addJulianYearsClip n =
    addJulianMonthsClip (n * 12)


{-| Add years, matching month and day, with Feb 29th rolled over to Mar 1st if
necessary.

For instance, 2004-02-29 + 2 years = 2006-03-01.
-}
addJulianYearsRollOver : Int -> Day -> Day
addJulianYearsRollOver n =
    addJulianMonthsRollOver (n * 12)


{-| Add months (clipped to last day), then add days
-}
addJulianDurationClip : CalendarDiffDays -> Day -> Day
addJulianDurationClip diffDays day =
    addDays (cdDays diffDays) (addJulianMonthsClip (cdMonths diffDays) day)


{-| Add months (rolling over to next month), then add days
-}
addJulianDurationRollOver : CalendarDiffDays -> Day -> Day
addJulianDurationRollOver diffDays day =
    addDays (cdDays diffDays) (addJulianMonthsRollOver (cdMonths diffDays) day)


{-| Calendrical difference, with as many whole months as possible
-}
diffJulianDurationClip : Day -> Day -> CalendarDiffDays
diffJulianDurationClip day2 day1 =
    let
        ( y1, m1, d1 ) =
            toJulian day1

        ( y2, m2, d2 ) =
            toJulian day2

        ym1 =
            y1 * 12 + m1

        ym2 =
            y2 * 12 + m2

        ymdiff =
            ym2 - ym1

        ymAllowed =
            if Days.toInt day2 >= Days.toInt day1 then
                if d2 >= d1 then
                    ymdiff
                else
                    ymdiff - 1
            else if d2 <= d1 then
                ymdiff
            else
                ymdiff + 1

        dayAllowed =
            addJulianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in
        CalendarDiffDays ymAllowed (diffDays day2 dayAllowed)


{-| Calendrical difference, with as many whole months as possible.

Same as `diffJulianDurationClip` for positive durations.
-}
diffJulianDurationRollOver : Day -> Day -> CalendarDiffDays
diffJulianDurationRollOver day2 day1 =
    let
        ( y1, m1, d1 ) =
            toJulian day1

        ( y2, m2, d2 ) =
            toJulian day2

        ym1 =
            y1 * 12 + m1

        ym2 =
            y2 * 12 + m2

        ymdiff =
            ym2 - ym1

        ymAllowed =
            if Days.toInt day2 >= Days.toInt day1 then
                if d2 >= d1 then
                    ymdiff
                else
                    ymdiff - 1
            else if d2 <= d1 then
                ymdiff
            else
                ymdiff + 1

        dayAllowed =
            addJulianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in
        CalendarDiffDays ymAllowed (diffDays day2 dayAllowed)
