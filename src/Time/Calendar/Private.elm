module Time.Calendar.Private exposing (..)


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
            toString i

        pad ->
            if i < 0 then
                "-" ++ showPaddedNum pad (negate i)
            else
                showPadded pad (toString i)



--show2Fixed : Pico -> String
--show2Fixed x | x < 10 = '0':(showFixed True x)
--show2Fixed x = showFixed True x


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
    x % 100


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
