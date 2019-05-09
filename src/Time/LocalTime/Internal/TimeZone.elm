module Time.LocalTime.Internal.TimeZone exposing
    ( TimeZone(..)
    , hoursToTimeZone
    , minutesToTimeZone
    , timeZoneMinutes
    , timeZoneOffsetString
    , timeZoneOffsetString_
    , timeZoneOffsetString__
    , toString
    , utc
    )

import Time.Calendar.Private exposing (..)



--import Time.Clock.System
--import Time.Clock.POSIX
--import Time.Clock.Internal.UTCTime


{-| A TimeZone is a whole number of minutes offset from UTC, together with a
name and a "just for summer" flag.

  - The number of minutes offset from UTC. Positive means local time will be
    later in the day than UTC.
  - Is this time zone just persisting for the summer?
  - The name of the zone, typically a three- or four-letter acronym.

-}
type TimeZone
    = TimeZone Int Bool String


timeZoneMinutes : TimeZone -> Int
timeZoneMinutes tz =
    case tz of
        TimeZone m _ _ ->
            m


timeZoneSummerOnly : TimeZone -> Bool
timeZoneSummerOnly tz =
    case tz of
        TimeZone _ so _ ->
            so


timeZoneName : TimeZone -> String
timeZoneName tz =
    case tz of
        TimeZone _ _ name ->
            name


{-| Create a nameless non-summer timezone for this number of minutes.
-}
minutesToTimeZone : Int -> TimeZone
minutesToTimeZone m =
    TimeZone m False ""


{-| Create a nameless non-summer timezone for this number of hours.
-}
hoursToTimeZone : Int -> TimeZone
hoursToTimeZone i =
    minutesToTimeZone (60 * i)


showT : Bool -> PadOption -> Int -> String
showT pred opt t =
    if pred then
        let
            opt_ =
                case opt of
                    NoPad ->
                        NoPad

                    Pad i c ->
                        Pad (max 0 (i - 3)) c
        in
        showPaddedNum opt_ (t // 60) ++ ":" ++ show2 (modBy 60 t)

    else
        showPaddedNum opt ((t // 60) * 100 + modBy 60 t)


timeZoneOffsetString__ : Bool -> PadOption -> TimeZone -> String
timeZoneOffsetString__ colon opt (TimeZone t _ _) =
    if t < 0 then
        String.cons '-' (showT colon opt (negate t))

    else
        String.cons '+' (showT colon opt t)



{- | Text representing the offset of this timezone, such as \"-0800\" or
   \"+0400\" (like @%z@ in formatTime), with arbitrary padding.
-}


timeZoneOffsetString_ : Maybe Char -> TimeZone -> String
timeZoneOffsetString_ tz =
    case tz of
        Nothing ->
            timeZoneOffsetString__ False NoPad

        Just c ->
            timeZoneOffsetString__ False (Pad 4 c)



-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like @%z@ in formatTime).


timeZoneOffsetString : TimeZone -> String
timeZoneOffsetString =
    timeZoneOffsetString__ False (Pad 4 '0')


toString : TimeZone -> String
toString ((TimeZone _ _ name) as zone) =
    if name == "" then
        timeZoneOffsetString zone

    else
        name


{-| The UTC time zone.
-}
utc : TimeZone
utc =
    TimeZone 0 False "UTC"



--getTimeZoneCTime : CTime -> IO TimeZone
--getTimeZoneCTime ctime = with 0 (\pdst -> with nullPtr (\pcname -> do
--    secs <- get_current_timezone_seconds ctime pdst pcname
--    case secs of
--        0x80000000 -> fail "localtime_r failed"
--        _ -> do
--            dst <- peek pdst
--            cname <- peek pcname
--            name <- peekCString cname
--            return (TimeZone ((fromIntegral secs) // 60) (dst == 1) name)
--    ))
--toCTime : Int64 -> IO CTime
--toCTime t = let
--    tt = fromIntegral t
--    t_ = fromIntegral tt
--    -- there_s no instance Bounded CTime, so this is the easiest way to check for overflow
--    in if t_ == t then return ( CTime tt) else fail "Data.Time.LocalTime.Internal.TimeZone.toCTime: Overflow"
--{-| Get the local time-zone for a given time (varying as per summertime
--adjustments).
---}
--getTimeZoneSystem : SystemTime -> IO TimeZone
--getTimeZoneSystem t = do
--    ctime <- toCTime (systemSeconds t)
--    getTimeZoneCTime ctime
--{-| Get the local time-zone for a given time (varying as per summertime adjustments).
---}
--getTimeZone : UTCTime -> IO TimeZone
--getTimeZone t = do
--    ctime <- toCTime (floor (utcTimeToPOSIXSeconds t))
--    getTimeZoneCTime ctime
--{-| Get the current time-zone.
---}
--getCurrentTimeZone : IO TimeZone
--getCurrentTimeZone = getSystemTime >>= getTimeZoneSystem
