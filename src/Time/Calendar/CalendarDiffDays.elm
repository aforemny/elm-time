module Time.Calendar.CalendarDiffDays exposing (..)


type CalendarDiffDays
    = CalendarDiffDays Int Int


cdMonths : CalendarDiffDays -> Int
cdMonths diffDays =
    case diffDays of
        CalendarDiffDays m _ ->
            m


cdDays : CalendarDiffDays -> Int
cdDays diffDays =
    case diffDays of
        CalendarDiffDays _ d ->
            d


calendarDay : CalendarDiffDays
calendarDay =
    CalendarDiffDays 0 1


calendarWeek : CalendarDiffDays
calendarWeek =
    CalendarDiffDays 0 7


calendarMonth : CalendarDiffDays
calendarMonth =
    CalendarDiffDays 1 0


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
