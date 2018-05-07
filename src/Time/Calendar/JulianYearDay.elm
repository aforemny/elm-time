module Time.Calendar.JulianYearDay exposing (..)

{-| TODO

@docs fromJulianYearAndDay, fromJulianYearAndDayValid
@docs isJulianLeapYear
@docs showJulianYearAndDay, toJulianYearAndDay
-}

import Time.Calendar.Days exposing (..)
import Time.Calendar.Private exposing (..)


{-| Convert to proleptic Julian year and day format. First element of result is
year (proleptic Julian calendar), second is the day of the year, with 1 for Jan
1, and 365 (or 366 in leap years) for Dec 31.
-}
toJulianYearAndDay : Day -> ( Int, Int )
toJulianYearAndDay date =
    case date of
        ModifiedJulianDay mjd ->
            let
                a =
                    mjd + 678577

                quad =
                    a // 1461

                d =
                    a % 1461

                y =
                    min (d // 365) 3

                yd =
                    d - (y * 365) + 1

                year =
                    quad * 4 + y + 1
            in
                ( year, yd )


{-| Convert from proleptic Julian year and day format.

Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
-}
fromJulianYearAndDay : Int -> Int -> Day
fromJulianYearAndDay year day =
    let
        y =
            year - 1

        mjd =
            (clip 1
                (if isJulianLeapYear year then
                    366
                 else
                    365
                )
                day
            )
                + (365 * y)
                + (y // 4)
                - 678578
    in
        (ModifiedJulianDay mjd)


{-| Convert from proleptic Julian year and day format.

Invalid day numbers will return Nothing
-}
fromJulianYearAndDayValid : Int -> Int -> Maybe Day
fromJulianYearAndDayValid year day =
    clipValid 1
        (if isJulianLeapYear year then
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
                        day_ + (365 * y) + (y // 4) - 678578
                in
                    ModifiedJulianDay mjd
            )


{-| Show in proleptic Julian year and day format (yyyy-ddd)
-}
showJulianYearAndDay : Day -> String
showJulianYearAndDay date =
    let
        ( y, d ) =
            toJulianYearAndDay date
    in
        (show4 y) ++ "-" ++ (show3 d)


{-| Is this year a leap year according to the proleptic Julian calendar?
-}
isJulianLeapYear : Int -> Bool
isJulianLeapYear year =
    (year % 4 == 0)
