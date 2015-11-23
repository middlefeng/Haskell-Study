


module PrettyJSON
(
    renderJValue
)
where


import Data.List (zipWith, replicate, lookup)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

import Numeric (showHex)
import Prettify
import HJson



string :: String -> Doc

string = enclose '"' '"' . hcat . map oneChar



oneChar :: Char -> Doc

oneChar c = case lookup c simpleEscapes of
                Just r      -> text r
                Nothing | mustEscape c  -> hexEscape c
                        | otherwise     -> char c
                        where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'




smallHex :: Int -> Doc

smallHex x = let h = showHex x "" in
                (text "\\u") <> (text (replicate (4 - length h) '0')) <> text h



astral :: Int -> Doc

astral x = let a = (x `shiftR` 10) .&. 0x3ff
               b = x .&. 0x3ff in
                    smallHex (a + 0xd800) <> smallHex (b + 0xdc00)


hexEscape :: Char -> Doc

hexEscape x | d < 0x10000   = smallHex d
            | otherwise     = astral (d - 0x10000)
                where d = ord x




simpleEscapes :: [(Char, String)]

simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                        where ch a b = (a, ['\\', b])



enclose :: Char -> Char -> Doc -> Doc

enclose left right x = (char left) <> x <> (char right) 



series :: Char -> Char -> (a -> Doc) -> [a] -> Doc

series open close item = enclose open close . fsep . punctuate (char ',') . map item



renderJValue :: JValue -> Doc

renderJValue (JString s)            = string s
renderJValue (JNumber n)            = double n
renderJValue (JNull)                = text "null"
renderJValue (JBool b)  | b         = text "true"
                        | otherwise = text "false"

renderJValue (JArray ary) = series '[' ']' renderJValue ary

renderJValue (JObject obj) = let field (name, value) = string name <>
                                                       text ": " <>
                                                       (renderJValue value) in
                                        series '{' '}' field obj


