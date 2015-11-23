

module HJson
(
    JValue (..),
    getString
) where


import Data.List (intercalate)


data JValue =   JString String
              | JNumber Double
              | JBool   Bool
              | JNull
              | JObject [(String, JValue)]
              | JArray  [JValue]
                deriving (Eq, Ord, Show)


getString :: JValue -> Maybe String

getString (JString s)   = Just s
getString _             = Nothing


renderJValue :: JValue -> String

renderJValue (JString s)    = show s
renderJValue (JNumber n)    = show n
renderJValue (JNull)        = "null"

renderJValue (JBool b) | b  = "True"
                       | otherwise = "False"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
                            where pairs [] = ""
                                  pairs ps = intercalate ", " (map renderPair ps) where
                                    renderPair (n, v) = show n ++ ": " ++ (renderJValue v)

renderJValue (JArray a) = "[" ++ values a ++ "]" where
                            values [] = ""
                            values as = intercalate ", " (map renderJValue as)

