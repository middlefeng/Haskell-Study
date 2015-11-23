

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Typeclassf
(
--  constraintDemo  ,
    Color(..)
)
where



class BasicEq a where

    isEqual :: a -> a -> Bool
    isEqual x y = not (isNotEqual x y)

    isNotEqual :: a -> a -> Bool
    isNotEqual x y = not (isEqual x y)


data Color =    Red
              | Green
              | Blue
                deriving (Read, Show)


type Astring = [Char]


instance BasicEq Astring where
    isEqual = ( == ) 



{-
constraintDemo :: String -> a

constraintDemo "a" = "12"
constraintDemo x = 12
-}

