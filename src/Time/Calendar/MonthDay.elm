module Time.Calendar.MonthDay exposing
    ( dayOfYearToMonthAndDay
    , monthAndDayToDayOfYear
    , monthAndDayToDayOfYearValid
    , monthLength
    )

import Time.Calendar.Private exposing (..)


{-| Convert month and day in the Gregorian or Julian calendars to day of year.
First arg is leap year flag.
-}
monthAndDayToDayOfYear : Bool -> Int -> Int -> Int
monthAndDayToDayOfYear isLeap month day =
    let
        month_ = clip 1 12 month
        day_ = clip 1 (monthLength_ isLeap month_) day
        k = if month_ <= 2 then 0 else if isLeap then -1 else -2
    in
    ((367 * month_ - 362) // 12) + k + day_


{-| Convert month and day in the Gregorian or Julian calendars to day of year.
First arg is leap year flag.
-}
monthAndDayToDayOfYearValid : Bool -> Int -> Int -> Maybe Int
monthAndDayToDayOfYearValid isLeap month day =
    clipValid 1 12 month
    |> Maybe.andThen (\month_ ->
      clipValid 1 (monthLength_ isLeap month_) day
      |> Maybe.map (\day_ ->
        let
            k = if month_ <= 2 then 0 else if isLeap then -1 else -2
        in
        ((367 * month_ - 362) // 12) + k + day_
      )
    )


{-| Convert day of year in the Gregorian or Julian calendars to month and day.
First arg is leap year flag.
-}
dayOfYearToMonthAndDay : Bool -> Int -> (Int,Int)
dayOfYearToMonthAndDay isLeap yd =
    findMonthDay (monthLengths isLeap) (clip 1 (if isLeap then 366 else 365) yd)


findMonthDay : List Int -> Int -> (Int,Int)
findMonthDay nss yd =
  case nss of
    (n :: ns) ->
      if yd > n then
          (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
      else
        (1, yd)
    _ ->
      (1, yd)


{-| The length of a given month in the Gregorian or Julian calendars.
First arg is leap year flag.
-}
monthLength : Bool -> Int -> Int
monthLength isLeap month_ =
    monthLength_ isLeap (clip 1 12 month_)


monthLength_ : Bool -> Int -> Int
monthLength_ isLeap month_ =
    monthLengths isLeap
    |> List.drop (month_ - 1)
    |> List.head
    |> \ result ->
      case result of
        Just result ->
          result
        Nothing ->
          Debug.crash "Time.Calendar.MonthDay.monthLength_"


monthLengths : Bool -> List Int
monthLengths isleap =
    [31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
    --J        F                   M  A  M  J  J  A  S  O  N  D
