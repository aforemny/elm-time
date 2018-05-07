module Time.Calendar.CalendarDiffDays exposing (..)

{-| TODO

@docs CalendarDiffDays
@docs cdDays, cdMonths
@docs calendarDay, calendarMonth, calendarWeek, calendarYear
@docs scaleCalendarDiffDays
-}


{-| TODO
-}
type CalendarDiffDays
    = CalendarDiffDays Int Int


{-| TODO
-}
cdMonths : CalendarDiffDays -> Int
cdMonths diffDays =
    case diffDays of
        CalendarDiffDays m _ ->
            m


{-| TODO
-}
cdDays : CalendarDiffDays -> Int
cdDays diffDays =
    case diffDays of
        CalendarDiffDays _ d ->
            d


{-| TODO
-}
calendarDay : CalendarDiffDays
calendarDay =
    CalendarDiffDays 0 1


{-| TODO
-}
calendarWeek : CalendarDiffDays
calendarWeek =
    CalendarDiffDays 0 7


{-| TODO
-}
calendarMonth : CalendarDiffDays
calendarMonth =
    CalendarDiffDays 1 0


{-| TODO
-}
calendarYear : CalendarDiffDays
calendarYear =
    CalendarDiffDays 12 0


{-| Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not
perfectly invert a duration, due to variable month lengths.
-}
scaleCalendarDiffDays : Int -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k diffDays =
    case diffDays of
        CalendarDiffDays m d ->
            CalendarDiffDays (k * m) (k * d)
