module Time.Calendar.Easter
    exposing
        ( sundayAfter
        , orthodoxPaschalMoon
        , orthodoxEaster
        , gregorianPaschalMoon
        , gregorianEaster
        )

{-| formulae from Reingold & Dershowitz, _Calendrical Calculations_, ch. 8.
-}

import Time.Calendar.CalendarDiffDays exposing (..)
import Time.Calendar.Days as Days exposing (..)
import Time.Calendar.Gregorian exposing (..)
import Time.Calendar.Julian exposing (..)
import Time.Calendar.Week exposing (..)


{-| The next Sunday strictly after a given day.
-}
sundayAfter : Day -> Day
sundayAfter day =
    addDays (7 - ((Days.toInt day + 3) % 7)) day


{-| Given a year, find the Paschal full moon according to Orthodox Christian
tradition
-}
orthodoxPaschalMoon : Int -> Day
orthodoxPaschalMoon year =
    let
        shiftedEpact =
            (14 + 11 * (year % 19)) % 30

        jyear =
            if year > 0 then
                year
            else
                year - 1
    in
        addDays (-shiftedEpact) (fromJulian jyear 4 19)


{-| Given a year, find Easter according to Orthodox Christian tradition
-}
orthodoxEaster : Int -> Day
orthodoxEaster =
    sundayAfter << orthodoxPaschalMoon


{-| Given a year, find the Paschal full moon according to the Gregorian method
-}
gregorianPaschalMoon : Int -> Day
gregorianPaschalMoon year =
    let
        century =
            (year // 100) + 1

        shiftedEpact =
            (14 + 11 * (year % 19) - ((3 * century) // 4) + ((5 + 8 * century) // 25)) % 30

        adjustedEpact =
            if shiftedEpact == 0 || ((shiftedEpact == 1) && (year % 19 < 10)) then
                shiftedEpact + 1
            else
                shiftedEpact
    in
        addDays (-adjustedEpact) (fromGregorian year 4 19)


{-| Given a year, find Easter according to the Gregorian method
-}
gregorianEaster : Int -> Day
gregorianEaster =
    sundayAfter << gregorianPaschalMoon
