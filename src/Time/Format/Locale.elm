-- Note: this file derives from old-locale:System.Locale.hs, which is copyright
-- (c) The University of Glasgow 2001


module Time.Format.Locale
    exposing
        ( TimeLocale(..)
        , defaultTimeLocale
        , iso8601DateFormat
        , rfc822DateFormat
        )

import Time.LocalTime.Internal.TimeZone exposing (..)


type TimeLocale
    = TimeLocale
        { -- full and abbreviated week days, starting with Sunday
          wDays :
            List ( String, String )
            -- full and abbreviated months
        , months :
            List ( String, String )
            -- AM\/PM symbols
        , amPm :
            ( String, String )
            -- formatting strings
        , dateTimeFmt : String
        , dateFmt : String
        , timeFmt : String
        , time12Fmt : String
        , -- time zones known by name
          knownTimeZones : List TimeZone
        }


{-| Locale representing American usage.

`knownTimeZones` contains only the ten time-zones mentioned in RFC 822 sec. 5:

UT, GMT, EST, EDT, CST, CDT, MST, MDT, PST, PDT.

Note that the parsing functions will regardless parse "UTC", single-letter
military time-zones, and +HHMM format.
-}
defaultTimeLocale : TimeLocale
defaultTimeLocale =
    TimeLocale
        { wDays =
            [ ( "Sunday", "Sun" )
            , ( "Monday", "Mon" )
            , ( "Tuesday", "Tue" )
            , ( "Wednesday", "Wed" )
            , ( "Thursday", "Thu" )
            , ( "Friday", "Fri" )
            , ( "Saturday", "Sat" )
            ]
        , months =
            [ ( "January", "Jan" )
            , ( "February", "Feb" )
            , ( "March", "Mar" )
            , ( "April", "Apr" )
            , ( "May", "May" )
            , ( "June", "Jun" )
            , ( "July", "Jul" )
            , ( "August", "Aug" )
            , ( "September", "Sep" )
            , ( "October", "Oct" )
            , ( "November", "Nov" )
            , ( "December", "Dec" )
            ]
        , amPm = ( "AM", "PM" )
        , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
        , dateFmt = "%m/%d/%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
            [ TimeZone 0 False "UT"
            , TimeZone 0 False "GMT"
            , TimeZone (-5 * 60) False "EST"
            , TimeZone (-4 * 60) True "EDT"
            , TimeZone (-6 * 60) False "CST"
            , TimeZone (-5 * 60) True "CDT"
            , TimeZone (-7 * 60) False "MST"
            , TimeZone (-6 * 60) True "MDT"
            , TimeZone (-8 * 60) False "PST"
            , TimeZone (-7 * 60) True "PDT"
            ]
        }


{-| Construct format string according to
<http://en.wikipedia.org/wiki/ISO_8601 ISO-8601>.

The `Maybe String` argument allows to supply an optional time specification.
E.g.:

```elm
'iso8601DateFormat' Nothing            == "%Y-%m-%d"           -- i.e. @/YYYY-MM-DD/@
'iso8601DateFormat' (Just "%H:%M:%S")  == "%Y-%m-%dT%H:%M:%S"  -- i.e. @/YYYY-MM-DD/T/HH:MM:SS/@
```
-}
iso8601DateFormat : Maybe String -> String
iso8601DateFormat mTimeFmt =
    (++)
        "%Y-%m-%d"
    <|
        case mTimeFmt of
            Nothing ->
                ""

            Just fmt ->
                String.cons 'T' fmt


{-| Format string according to <http://tools.ietf.org/html/rfc822#section-5 RFC822>.
-}
rfc822DateFormat : String
rfc822DateFormat =
    "%a, %_d %b %Y %H:%M:%S %Z"
