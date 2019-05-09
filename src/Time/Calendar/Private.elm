module Time.Calendar.Private exposing (PadOption(..), chopZeros, clip, clipValid, div100, mod100, show2, show2Fixed, show3, show4, showFixed, showIntegerZeros, showPadded, showPaddedNum, withDot)


type PadOption
    = Pad Int Char
    | NoPad


showPadded : PadOption -> String -> String
showPadded padOptions s =
    case padOptions of
        NoPad ->
            s

        Pad i c ->
            String.fromList (List.repeat (i - String.length s) c) ++ s


showPaddedNum : PadOption -> Int -> String
showPaddedNum padOption i =
    case padOption of
        NoPad ->
            String.fromInt i

        pad ->
            if i < 0 then
                "-" ++ showPaddedNum pad (negate i)

            else
                showPadded pad (String.fromInt i)


show2Fixed : Int -> Int -> String
show2Fixed res x =
    if x < 10 then
        String.cons '0' (showFixed res True x)

    else
        showFixed res True x


showFixed : Int -> Bool -> Int -> String
showFixed res chopTrailingZeros a =
    if a < 0 then
        "-" ++ showFixed res chopTrailingZeros -a

    else
        let
            ( i, d ) =
                ( a // res, modBy res a )

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
        String.fromInt i ++ withDot (showIntegerZeros chopTrailingZeros digits fracNum)


showIntegerZeros : Bool -> Int -> Int -> String
showIntegerZeros chopTrailingZeros digits a =
    if chopTrailingZeros && (a == 0) then
        ""

    else
        let
            s =
                String.fromInt a

            s_ =
                if chopTrailingZeros then
                    chopZeros a

                else
                    s
        in
        String.repeat (digits - String.length s) "0" ++ s_


chopZeros : Int -> String
chopZeros a =
    if a == 0 then
        ""

    else if modBy 10 a == 0 then
        chopZeros (a // 10)

    else
        String.fromInt a


withDot : String -> String
withDot s =
    if s == "" then
        s

    else
        String.cons '.' s


show2 : Int -> String
show2 =
    showPaddedNum (Pad 2 '0')


show3 : Int -> String
show3 =
    showPaddedNum (Pad 3 '0')


show4 : Int -> String
show4 =
    showPaddedNum (Pad 4 '0')


mod100 : Int -> Int
mod100 x =
    modBy 100 x


div100 : Int -> Int
div100 x =
    x // 100


clip : comparable -> comparable -> comparable -> comparable
clip a b x =
    if x < a then
        a

    else if x > b then
        b

    else
        x


clipValid : comparable -> comparable -> comparable -> Maybe comparable
clipValid a b x =
    if x < a then
        Nothing

    else if x > b then
        Nothing

    else
        Just x



--quotBy : (Real a,Integral b) => a -> a -> b
--quotBy d n = truncate ((toRational n) / (toRational d))
--
--remBy : Real a => a -> a -> a
--remBy d n = n - (fromInteger f) * d where
--    f = quotBy d n
--
--quotRemBy : (Real a,Integral b) => a -> a -> (b,a)
--quotRemBy d n = let
--    f = quotBy d n
--    in (f,n - (fromIntegral f) * d)
