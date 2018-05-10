module Time.Clock.Internal.DiffTime
    exposing
        ( DiffTime
        , diffTimeToPicoseconds
        , diffTimeToSeconds
        , equal
        , lessOrEqual
        , picosecondsToDiffTime
        , secondsToDiffTime
        , minus
        )

import Basics


{-| This is a length of time, as measured by a clock.
Conversion functions will treat it as seconds.
It has a precision of 10^-12 s.
-}
type DiffTime
    = MkDiffTime Int


equal a b =
  diffTimeToPicoseconds a == diffTimeToPicoseconds b

lessOrEqual a b =
  diffTimeToPicoseconds a <= diffTimeToPicoseconds b


minus : DiffTime -> DiffTime -> DiffTime
minus tod1 tod2 =
  picosecondsToDiffTime ((diffTimeToPicoseconds tod1) - (diffTimeToPicoseconds tod2))


picosecondsToDiffTime : Int -> DiffTime
picosecondsToDiffTime pico =
  MkDiffTime pico


diffTimeToPicoseconds : DiffTime -> Int
diffTimeToPicoseconds dt =
  case dt of
    MkDiffTime pico -> pico



-- necessary because H98 doesn't have "cunning newtype" derivation
--instance Enum DiffTime where
--    succ (MkDiffTime a) = MkDiffTime (succ a)
--    pred (MkDiffTime a) = MkDiffTime (pred a)
--    toEnum = MkDiffTime . toEnum
--    fromEnum (MkDiffTime a) = fromEnum a
--    enumFrom (MkDiffTime a) = fmap MkDiffTime (enumFrom a)
--    enumFromThen (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromThen a b)
--    enumFromTo (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromTo a b)
--    enumFromThenTo (MkDiffTime a) (MkDiffTime b) (MkDiffTime c) = fmap MkDiffTime (enumFromThenTo a b c)


toString : DiffTime -> String
toString diffTime =
    case diffTime of
        MkDiffTime t ->
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



--instance Num DiffTime where
--    (MkDiffTime a) + (MkDiffTime b) = MkDiffTime (a + b)
--    (MkDiffTime a) - (MkDiffTime b) = MkDiffTime (a - b)
--    (MkDiffTime a) * (MkDiffTime b) = MkDiffTime (a * b)
--    negate (MkDiffTime a) = MkDiffTime (negate a)
--    abs (MkDiffTime a) = MkDiffTime (abs a)
--    signum (MkDiffTime a) = MkDiffTime (signum a)
--    fromInteger i = MkDiffTime (fromInteger i)
--instance Real DiffTime where
--    toRational (MkDiffTime a) = toRational a
--instance Fractional DiffTime where
--    (MkDiffTime a) / (MkDiffTime b) = MkDiffTime (a / b)
--    recip (MkDiffTime a) = MkDiffTime (recip a)
--    fromRational r = MkDiffTime (fromRational r)
--instance RealFrac DiffTime where
--    properFraction (MkDiffTime a) = let (b',a') = properFraction a in (b',MkDiffTime a')
--    truncate (MkDiffTime a) = truncate a
--    round (MkDiffTime a) = round a
--    ceiling (MkDiffTime a) = ceiling a
--    floor (MkDiffTime a) = floor a


{-| Create a 'DiffTime' which represents an integral number of seconds.
-}
secondsToDiffTime : Int -> DiffTime
secondsToDiffTime =
    MkDiffTime << ((*) (10^12))


{-| Get the number of picoseconds in a 'DiffTime'.
-}
diffTimeToSeconds : DiffTime -> Int
diffTimeToSeconds diffTime =
    case diffTime of
        MkDiffTime x ->
            (x // (10^12))
