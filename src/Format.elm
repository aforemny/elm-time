module Format
    exposing
        ( casesFormat
        , clipFormat
        , decimalFormat
        , filterFormat
        , Format(..)
        , formatParseM
        , formatShow
        , integerFormat
        , literalFormat
        , mandatorySignFormat
        , mapMFormat
        , optionalFormat
        , optionalSignFormat
        , parseReader
        , SignOption(..)
        , specialCaseFormat
        , specialCaseShowFormat
        )

import Combine
import Combine.Num as Combine


type alias ReadP t =
    Combine.Parser () t


type alias Error =
    Combine.ParseErr ()


parseReader : ReadP t -> String -> Result Error t
parseReader readp s =
    Combine.parse readp s
        |> Result.map (\( _, _, result ) -> result)


{-| A text format for a type
-}
type Format t
    = MkFormat (t -> Maybe String) (ReadP t)


formatShowM : Format t -> (t -> Maybe String)
formatShowM format =
    case format of
        MkFormat formatShowM _ ->
            formatShowM


formatReadP : Format t -> ReadP t
formatReadP format =
    case format of
        MkFormat _ formatReadP ->
            formatReadP


{-| Show a value in the format, or error if unrepresentable
-}
formatShow : Format t -> t -> String
formatShow format t =
    case formatShowM format t of
        Just str ->
            str

        Nothing ->
            Debug.crash "Format.formatShow: bad value"


{-| Parse a value in the format
-}
formatParseM : Format t -> String -> Result Error t
formatParseM format =
    parseReader (formatReadP format)


mapMFormat : (a -> Maybe b) -> (b -> Maybe a) -> Format a -> Format b
mapMFormat amb bma format =
    case format of
        MkFormat sa ra ->
            MkFormat (\b -> bma b |> Maybe.andThen sa)
                (ra
                    |> Combine.andThen
                        (\a ->
                            case amb a of
                                Just b ->
                                    Combine.succeed b

                                Nothing ->
                                    Combine.fail ""
                        )
                )


filterFormat : (a -> Bool) -> Format a -> Format a
filterFormat test =
    mapMFormat
        (\a ->
            if test a then
                Just a
            else
                Nothing
        )
        (\a ->
            if test a then
                Just a
            else
                Nothing
        )


{-| Limits are inclusive
-}
clipFormat : ( comparable, comparable ) -> Format comparable -> Format comparable
clipFormat ( lo, hi ) =
    filterFormat (\a -> a >= lo && a <= hi)


literalFormat : String -> Format ()
literalFormat s =
    MkFormat
        (\_ -> Just s)
        (Combine.string s
            |> Combine.map (\_ -> ())
        )


specialCaseShowFormat : ( comparable, String ) -> Format comparable -> Format comparable
specialCaseShowFormat ( val, str ) (MkFormat s r) =
    let
        s_ t =
            if t == val then
                Just str
            else
                s t
    in
        MkFormat s_ r


specialCaseFormat : ( comparable, String ) -> Format comparable -> Format comparable
specialCaseFormat ( val, str ) (MkFormat s r) =
    let
        s_ t =
            if t == val then
                Just str
            else
                s t

        r_ =
            Combine.choice
                [ Combine.string str
                    |> Combine.map (\_ -> val)
                , r
                ]
    in
        MkFormat s_ r_


optionalFormat : comparable -> Format comparable -> Format comparable
optionalFormat val =
    specialCaseFormat ( val, "" )


casesFormat : List ( comparable, String ) -> Format comparable
casesFormat pairs =
    let
        s t =
            pairs
                |> List.filter ((==) t << Tuple.first)
                |> List.head
                |> Maybe.map Tuple.second

        r pps =
            case pps of
                [] ->
                    Combine.fail ""

                ( v, str ) :: pp ->
                    Combine.choice
                        [ Combine.string str
                            |> Combine.map (\_ -> v)
                        , r pp
                        ]
    in
        MkFormat s (r pairs)


optionalSignFormat : Format number
optionalSignFormat =
    casesFormat
        [ ( 1, "" )
        , ( 1, "+" )
        , ( 0, "" )
        , ( -1, "-" )
        ]


mandatorySignFormat : Format number
mandatorySignFormat =
    casesFormat
        [ ( 1, "+" )
        , ( 0, "+" )
        , ( -1, "-" )
        ]


type SignOption
    = NoSign
    | NegSign
    | PosNegSign


readSign : SignOption -> ReadP (number -> number)
readSign signOption =
    case signOption of
        NoSign ->
            Combine.succeed identity

        NegSign ->
            Combine.optional identity (Combine.string "-" |> Combine.map (\_ n -> (-1) * n))

        PosNegSign ->
            Combine.choice
                [ Combine.string "+" |> Combine.map (\_ -> identity)
                , Combine.string "-" |> Combine.map (\_ n -> (-1) * n)
                ]


readInteger : SignOption -> Maybe Int -> Bool -> ReadP Int
readInteger =
    readNumber_
        (\s ->
            case String.toInt s of
                Err _ ->
                    Debug.crash "Format.readInteger: bad value"

                Ok result ->
                    result
        )


readDecimal : SignOption -> Maybe Int -> Bool -> ReadP Float
readDecimal =
    readNumber_
        (\s ->
            case String.toFloat s of
                Err _ ->
                    Debug.crash "Format.readInteger: bad value"

                Ok result ->
                    result
        )


readNumber_ : (String -> number) -> SignOption -> Maybe Int -> Bool -> ReadP number
readNumber_ read signOpt mdigitcount allowDecimal =
    readSign signOpt
        |> Combine.andThen
            (\sign ->
                (case mdigitcount of
                    Just digitcount ->
                        Combine.count digitcount Combine.digit

                    Nothing ->
                        Combine.many1 Combine.digit
                )
                    |> Combine.map (List.map toString)
                    |> Combine.map String.concat
                    |> Combine.andThen
                        (\digits ->
                            (if allowDecimal then
                                Combine.optional ""
                                    (Combine.choice [ Combine.string ".", Combine.string "," ]
                                        |> Combine.andThen
                                            (\_ ->
                                                Combine.many1 Combine.digit
                                                    |> Combine.map (List.map toString)
                                                    |> Combine.map String.concat
                                                    |> Combine.andThen
                                                        (\dd ->
                                                            Combine.succeed (String.cons '.' dd)
                                                        )
                                            )
                                    )
                             else
                                Combine.succeed ""
                            )
                                |> Combine.andThen
                                    (\moredigits ->
                                        Combine.succeed (sign (read (digits ++ moredigits)))
                                    )
                        )
            )


zeroPad : Maybe Int -> String -> String
zeroPad i_ s =
    case i_ of
        Nothing ->
            s

        Just i ->
            String.repeat (i - String.length s) "0" ++ s


trimTrailing : String -> String
trimTrailing s =
    case s of
        "" ->
            ""

        "." ->
            ""

        _ ->
            if String.dropLeft (String.length s - 1) s == "0" then
                trimTrailing (String.left (String.length s - 1) s)
            else
                s


showNumber : SignOption -> Maybe Int -> number -> Maybe String
showNumber signOpt mdigitcount t =
    let
        showIt str =
            let
                ( intPart, decPart ) =
                    case String.split "." str of
                        [] ->
                            ( "", "" )

                        intPart :: decParts ->
                            ( intPart, String.join "." decParts )
            in
                (zeroPad mdigitcount intPart) ++ trimTrailing decPart

        str =
            toString t
    in
        case String.uncons str of
            Just ( '-', str ) ->
                case signOpt of
                    NoSign ->
                        Nothing

                    _ ->
                        Just (String.cons '-' (showIt str))

            _ ->
                Just <|
                    case signOpt of
                        PosNegSign ->
                            String.cons '+' (showIt str)

                        _ ->
                            showIt str


integerFormat : SignOption -> Maybe Int -> Format Int
integerFormat signOpt mdigitcount =
    MkFormat (showNumber signOpt mdigitcount) (readInteger signOpt mdigitcount False)


decimalFormat : SignOption -> Maybe Int -> Format Float
decimalFormat signOpt mdigitcount =
    MkFormat (showNumber signOpt mdigitcount) (readDecimal signOpt mdigitcount True)
