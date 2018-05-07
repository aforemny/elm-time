module Time.Calendar.Days exposing
    ( addDays
    , Day(..)
    , diffDays
    , fromInt
    , inRange
    , pred
    , range
    , succ
    , toInt
    )
{-|
@docs Day
@docs addDays, diffDays
@docs pred, succ, fromInt, toInt
@docs range, inRange
-}

{-| The Modified Julian Day is a standard count of days, with zero being the
day 1858-11-17.
-}
type Day =
      ModifiedJulianDay Int


toModifiedJulianDay : Day -> Int
toModifiedJulianDay date =
    case date of
        ModifiedJulianDay a -> a


{-| TODO
-}
succ : Day -> Day
succ date =
    case date of
        ModifiedJulianDay a -> ModifiedJulianDay (a+1)


{-| TODO
-}
pred : Day -> Day
pred date =
    case date of
        ModifiedJulianDay a -> ModifiedJulianDay (a-1)


{-| TODO
-}
fromInt : Int -> Day
fromInt a =
    ModifiedJulianDay a


{-| TODO
-}
toInt : Day -> Int
toInt date =
    case date of
        ModifiedJulianDay a -> a


{-| TODO
-}
range : ( Day, Day ) -> List Day
range ( a, b ) =
    List.map fromInt (List.range (toInt a) (toInt b))


{-| TODO
-}
inRange : ( Day, Day ) -> Day -> Bool
inRange ( a, b ) c =
    (toInt a <= toInt c) && (toInt c <= toInt b)


{-| TODO
-}
addDays : Int -> Day -> Day
addDays n a =
    fromInt (toInt a + n)


{-| TODO
-}
diffDays : Day -> Day -> Int
diffDays a b =
    toInt a - toInt b
