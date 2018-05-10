module Time.Clock.Internal.NominalDiffTime
    exposing
        ( nominalDay
        , NominalDiffTime
        , nominalDiffTimeToSeconds
        , secondsToNominalDiffTime
        )

import Time.Calendar.Days as Days exposing (Day)


{-| This is a length of time, as measured by UTC.
Conversion functions will treat it as seconds.
It has a precision of 10^-12 s.
It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
For instance, 23:00 UTC + 2 hours of NominalDiffTime = 01:00 UTC (+ 1 day),
regardless of whether a leap-second intervened.
-}
type NominalDiffTime
    = MkNominalDiffTime Int


nominalDiffTimeToPicoseconds ndt =
    case ndt of
        MkNominalDiffTime pico ->
            pico


picosecondsToNominalDiffTime pico =
    MkNominalDiffTime pico


equal a b =
    nominalDiffTimeToPicoseconds a == nominalDiffTimeToPicoseconds b


lessOrEqual a b =
    nominalDiffTimeToPicoseconds a <= nominalDiffTimeToPicoseconds b


{-| Create a 'NominalDiffTime' from a number of seconds.
-}
secondsToNominalDiffTime : Int -> NominalDiffTime
secondsToNominalDiffTime seconds =
    picosecondsToNominalDiffTime (seconds * 10^12)



-- | Get the seconds in a 'NominalDiffTime'.


nominalDiffTimeToSeconds : NominalDiffTime -> Int
nominalDiffTimeToSeconds diffTime =
    case diffTime of
        MkNominalDiffTime t ->
            (t // 10^12)



--instance Enum NominalDiffTime where
--    succ (MkNominalDiffTime a) = MkNominalDiffTime (succ a)
--    pred (MkNominalDiffTime a) = MkNominalDiffTime (pred a)
--    toEnum = MkNominalDiffTime . toEnum
--    fromEnum (MkNominalDiffTime a) = fromEnum a
--    enumFrom (MkNominalDiffTime a) = fmap MkNominalDiffTime (enumFrom a)
--    enumFromThen (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromThen a b)
--    enumFromTo (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromTo a b)
--    enumFromThenTo (MkNominalDiffTime a) (MkNominalDiffTime b) (MkNominalDiffTime c) = fmap MkNominalDiffTime (enumFromThenTo a b c)


toString (MkNominalDiffTime t) =
    (showFixed True t) ++ "s"


showFixed : Bool -> Int -> String
showFixed chopTrailingZeros a =
    if a < 0 then
        "-" ++ showFixed chopTrailingZeros (negate a)
    else
        let
            res =
                1000000000000

            ( i, d ) =
                ( a // res, a % res )

            -- enough digits to be unambiguous
            digits =
                ceiling (logBase 10 (toFloat res))

            maxnum =
                10 ^ digits

            -- read floors, so show must ceil for `read . show = id` to hold. See #9240
            fracNum =
                divCeil (d * maxnum) res

            divCeil x y =
                (x + y - 1) // y
        in
            (Basics.toString i) ++ (withDot (showIntegerZeros chopTrailingZeros digits fracNum))


showIntegerZeros : Bool -> Int -> Int -> String
showIntegerZeros chopTrailingZeros digits a =
    if chopTrailingZeros && (a == 0) then
        ""
    else
        let
            s =
                Basics.toString a

            s_ =
                if chopTrailingZeros then
                    chopZeros a
                else
                    s
        in
            String.repeat (digits - String.length s) "0" ++ s_


chopZeros : Int -> String
chopZeros a =
    case a of
        0 ->
            ""

        _ ->
            if a % 10 == 0 then
                chopZeros (a // 10)
            else
                Basics.toString a


withDot : String -> String
withDot s =
    case s of
        "" ->
            ""

        _ ->
            "." ++ s



--instance Num NominalDiffTime where
--    (MkNominalDiffTime a) + (MkNominalDiffTime b) = MkNominalDiffTime (a + b)
--    (MkNominalDiffTime a) - (MkNominalDiffTime b) = MkNominalDiffTime (a - b)
--    (MkNominalDiffTime a) * (MkNominalDiffTime b) = MkNominalDiffTime (a * b)
--    negate (MkNominalDiffTime a) = MkNominalDiffTime (negate a)
--    abs (MkNominalDiffTime a) = MkNominalDiffTime (abs a)
--    signum (MkNominalDiffTime a) = MkNominalDiffTime (signum a)
--    fromInteger i = MkNominalDiffTime (fromInteger i)
--instance Real NominalDiffTime where
--    toRational (MkNominalDiffTime a) = toRational a
--instance Fractional NominalDiffTime where
--    (MkNominalDiffTime a) / (MkNominalDiffTime b) = MkNominalDiffTime (a / b)
--    recip (MkNominalDiffTime a) = MkNominalDiffTime (recip a)
--    fromRational r = MkNominalDiffTime (fromRational r)
--instance RealFrac NominalDiffTime where
--    properFraction (MkNominalDiffTime a) = (i,MkNominalDiffTime f) where
--        (i,f) = properFraction a
--    truncate (MkNominalDiffTime a) = truncate a
--    round (MkNominalDiffTime a) = round a
--    ceiling (MkNominalDiffTime a) = ceiling a
--    floor (MkNominalDiffTime a) = floor a
-- | One day in 'NominalDiffTime'.


nominalDay : NominalDiffTime
nominalDay =
    secondsToNominalDiffTime 86400
