module Time.Calendar.Gregorian
    exposing
        ( toGregorian
        , fromGregorian
        , fromGregorianValid
        , showGregorian
        , gregorianMonthLength
        , addGregorianMonthsClip
        , addGregorianMonthsRollOver
        , addGregorianYearsClip
        , addGregorianYearsRollOver
        , addGregorianDurationClip
        , addGregorianDurationRollOver
        , diffGregorianDurationClip
        , diffGregorianDurationRollOver
        )

import Time.Calendar.MonthDay exposing (..)
import Time.Calendar.OrdinalDate exposing (..)
import Time.Calendar.Days as Days exposing (..)
import Time.Calendar.CalendarDiffDays exposing (..)
import Time.Calendar.Private exposing (..)


{-| Convert to proleptic Gregorian calendar. First element of result is year,
second month number (1-12), third day (1-31).
-}
toGregorian : Day -> ( Int, Int, Int )
toGregorian date =
    let
        ( year, yd ) =
            toOrdinalDate date

        ( month, day ) =
            dayOfYearToMonthAndDay (isLeapYear year) yd
    in
        ( year, month, day )


{-| Convert from proleptic Gregorian calendar. First argument is year, second
month number (1-12), third day (1-31).

Invalid values will be clipped to the correct range, month first, then day.
-}
fromGregorian : Int -> Int -> Int -> Day
fromGregorian year month day =
    fromOrdinalDate year (monthAndDayToDayOfYear (isLeapYear year) month day)


{-| Convert from proleptic Gregorian calendar. First argument is year, second
month number (1-12), third day (1-31).

Invalid values will return Nothing
-}
fromGregorianValid : Int -> Int -> Int -> Maybe Day
fromGregorianValid year month day =
    monthAndDayToDayOfYearValid (isLeapYear year) month day
        |> Maybe.andThen
            (\doy ->
                fromOrdinalDateValid year doy
            )


{-| Show in ISO 8601 format (yyyy-mm-dd)
-}
showGregorian : Day -> String
showGregorian date =
    let
        ( y, m, d ) =
            toGregorian date
    in
        (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)


{-| The number of days in a given month according to the proleptic Gregorian
calendar. First argument is year, second is month.
-}
gregorianMonthLength : Int -> Int -> Int
gregorianMonthLength year =
    monthLength (isLeapYear year)


rolloverMonths : ( Int, Int ) -> ( Int, Int )
rolloverMonths ( y, m ) =
    ( y + ((m - 1) // 12), ((m - 1) % 12) + 1 )


addGregorianMonths : Int -> Day -> ( Int, Int, Int )
addGregorianMonths n day =
    let
        ( y, m, d ) =
            toGregorian day

        ( y_, m_ ) =
            rolloverMonths ( y, m + n )
    in
        ( y_, m_, d )


{-| Add months, with days past the last day of the month clipped to the last
day.

For instance, 2005-01-30 + 1 month = 2005-02-28.
-}
addGregorianMonthsClip : Int -> Day -> Day
addGregorianMonthsClip n day =
    let
        ( y, m, d ) =
            addGregorianMonths n day
    in
        fromGregorian y m d


{-| Add months, with days past the last day of the month rolling over to the
next month.

For instance, 2005-01-30 + 1 month = 2005-03-02.
-}
addGregorianMonthsRollOver : Int -> Day -> Day
addGregorianMonthsRollOver n day =
    let
        ( y, m, d ) =
            addGregorianMonths n day
    in
        addDays (d - 1) (fromGregorian y m 1)


{-| Add years, matching month and day, with Feb 29th clipped to Feb 28th if
  necessary.

For instance, 2004-02-29 + 2 years = 2006-02-28.
-}
addGregorianYearsClip : Int -> Day -> Day
addGregorianYearsClip n =
    addGregorianMonthsClip (n * 12)


{-| Add years, matching month and day, with Feb 29th rolled over to Mar 1st if
necessary.

For instance, 2004-02-29 + 2 years = 2006-03-01.
-}
addGregorianYearsRollOver : Int -> Day -> Day
addGregorianYearsRollOver n =
    addGregorianMonthsRollOver (n * 12)


{-| Add months (clipped to last day), then add days
-}
addGregorianDurationClip : CalendarDiffDays -> Day -> Day
addGregorianDurationClip diffDays day =
    addDays (cdDays diffDays) (addGregorianMonthsClip (cdMonths diffDays) day)


{-| Add months (rolling over to next month), then add days
-}
addGregorianDurationRollOver : CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver diffDays day =
    addDays (cdDays diffDays) (addGregorianMonthsRollOver (cdMonths diffDays) day)


{-| Calendrical difference, with as many whole months as possible
-}
diffGregorianDurationClip : Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 =
    let
        ( y1, m1, d1 ) =
            toGregorian day1

        ( y2, m2, d2 ) =
            toGregorian day2

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
            addGregorianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in
        CalendarDiffDays ymAllowed (diffDays day2 dayAllowed)


{-| Calendrical difference, with as many whole months as possible.

Same as 'diffGregorianDurationClip' for positive durations.
-}
diffGregorianDurationRollOver : Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 =
    let
        ( y1, m1, d1 ) =
            toGregorian day1

        ( y2, m2, d2 ) =
            toGregorian day2

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
            addGregorianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in
        CalendarDiffDays ymAllowed (diffDays day2 dayAllowed)
