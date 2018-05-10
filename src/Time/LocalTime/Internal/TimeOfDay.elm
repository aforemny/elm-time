module Time.LocalTime.Internal.TimeOfDay
    exposing
        ( dayFractionToTimeOfDay
        , daysAndTimeOfDayToTime
        , hours
        , localToUTCTimeOfDay
        , makeTimeOfDayValid
        , midday
        , midnight
        , minutes
        , seconds
        , TimeOfDay(..)
        , timeOfDayToDayFraction
        , timeOfDayToTime
        , timeToDaysAndTimeOfDay
        , timeToTimeOfDay
        , utcToLocalTimeOfDay
        )

import Time.Calendar.Private as Calendar exposing (..)
import Time.Clock.Internal.DiffTime as DiffTime exposing (..)
import Time.Clock.Internal.NominalDiffTime as NominalDiffTime exposing (..)
import Time.LocalTime.Internal.TimeZone as TimeZone exposing (..)


{-| Time of day as represented in hour, minute and second (with picoseconds),
typically used to express local time of day.
-}
type TimeOfDay
    = TimeOfDay Int Int Int


hours : TimeOfDay -> Int
hours tod =
    case tod of
        TimeOfDay hours _ _ ->
            hours


minutes : TimeOfDay -> Int
minutes tod =
    case tod of
        TimeOfDay _ minutes _ ->
            minutes


seconds : TimeOfDay -> Int
seconds tod =
    case tod of
        TimeOfDay _ _ seconds ->
            seconds


equal : TimeOfDay -> TimeOfDay -> Bool
equal tod1 tod2 =
    DiffTime.equal (timeOfDayToTime tod1) (timeOfDayToTime tod2)


lessOrEqual : TimeOfDay -> TimeOfDay -> Bool
lessOrEqual tod1 tod2 =
    DiffTime.lessOrEqual (timeOfDayToTime tod1) (timeOfDayToTime tod2)


{-| Hour zero
-}
midnight : TimeOfDay
midnight =
    TimeOfDay 0 0 0


{-| Hour twelve
-}
midday : TimeOfDay
midday =
    TimeOfDay 12 0 0


toString : TimeOfDay -> String
toString tod =
    case tod of
        TimeOfDay h m s ->
            (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed (10^12) s)


makeTimeOfDayValid : Int -> Int -> Int -> Maybe TimeOfDay
makeTimeOfDayValid h m s =
    Maybe.map3
        TimeOfDay
        (clipValid 0 23 h)
        (clipValid 0 59 m)
        (Maybe.map floor (clipValid 0 60.999999999999 (toFloat s)))


{-| Convert a period of time into a count of days and a time of day since midnight.
The time of day will never have a leap second.
-}
timeToDaysAndTimeOfDay : NominalDiffTime -> ( Int, TimeOfDay )
timeToDaysAndTimeOfDay dt =
    let
        s =
            nominalDiffTimeToSeconds dt

        ( m, ms ) =
            ( s // 60, s % 60 )

        ( h, hm ) =
            ( m // 60, m % 60 )

        ( d, dh ) =
            ( h // 24, h % 24 )
    in
        ( d, TimeOfDay dh hm ms )


{-| Convert a count of days and a time of day since midnight into a period of
  time.
-}
daysAndTimeOfDayToTime : Int -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d tod =
    case tod of
        TimeOfDay dh hm ms ->
          secondsToNominalDiffTime <|
            (+) (ms) <|
                (*) 60 <|
                    (+) (hm) <|
                        (*) 60 <|
                            (+) (dh) <|
                                (*) 24 <|
                                    d


{-| Convert a time of day in UTC to a time of day in some timezone, together
with a day adjustment.
-}
utcToLocalTimeOfDay : TimeZone -> TimeOfDay -> ( Int, TimeOfDay )
utcToLocalTimeOfDay zone tod =
    case tod of
        TimeOfDay h m s ->
            let
                m_ =
                    m + timeZoneMinutes zone

                h_ =
                    h + (m_ // 60)
            in
                ( (h_ // 24), TimeOfDay (h_ % 24) (m_ % 60) s )


{-| Convert a time of day in some timezone to a time of day in UTC, together
with a day adjustment.
-}
localToUTCTimeOfDay : TimeZone -> TimeOfDay -> ( Int, TimeOfDay )
localToUTCTimeOfDay zone =
    utcToLocalTimeOfDay (minutesToTimeZone (-(timeZoneMinutes zone)))


posixDayLength : DiffTime
posixDayLength =
    secondsToDiffTime 86400


{-| Get the time of day given a time since midnight.
Time more than 24h will be converted to leap-seconds.
-}
timeToTimeOfDay : DiffTime -> TimeOfDay
timeToTimeOfDay dt =
    if diffTimeToSeconds dt >= diffTimeToSeconds posixDayLength then
        TimeOfDay 23 59 (60 + (diffTimeToSeconds (DiffTime.minus dt posixDayLength)))
    else
        let
            s_ =
                diffTimeToSeconds dt

            s =
                s_ % 60

            m_ =
                s_ // 60

            m =
                m_ % 60

            h =
                m_ // 60
        in
            TimeOfDay h m s


{-| Get the time since midnight for a given time of day.
-}
timeOfDayToTime : TimeOfDay -> DiffTime
timeOfDayToTime tod =
    case tod of
        TimeOfDay h m s ->
            secondsToDiffTime ((h * 60 + m) * 60 + s)


{-| Get the time of day given the fraction of a day since midnight.
-}
dayFractionToTimeOfDay : Float -> TimeOfDay
dayFractionToTimeOfDay df =
    timeToTimeOfDay (secondsToDiffTime (floor (df * 86400)))


{-| Get the fraction of a day since midnight given a time of day.
-}
timeOfDayToDayFraction : TimeOfDay -> Float
timeOfDayToDayFraction tod =
    toFloat (diffTimeToSeconds (timeOfDayToTime tod)) / toFloat (diffTimeToSeconds posixDayLength)
