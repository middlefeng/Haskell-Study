

module Returntype
(
    TempratureUnit,
    Kelvin,
    Celcius,
    fromKelvin
)
where


data Kelvin = Kelvin Float deriving Show
data Celcius = Celcius Float deriving Show
data Fahrenheit = Fahrenheit Float deriving Show



fromKelvin :: (TempratureUnit TempUnit2 b) => Kelvin -> b

fromKelvin = fromKelvinT




class TempUnit2 a where

    test :: a -> Float



instance TempUnit2 Celcius where

    test (Celcius a) = a





class TempratureUnit a where

    fromKelvinT :: Kelvin -> a
    toKelvin :: a -> Kelvin



instance TempratureUnit Celcius where

    fromKelvinT (Kelvin k) = Celcius (k - 273.15)
    toKelvin (Celcius c) = Kelvin (c + 273.15) 


instance TempratureUnit Fahrenheit where

    fromKelvinT (Kelvin k) = Fahrenheit ((k-273.15)*1.8 + 32)
    toKelvin (Fahrenheit f) = Kelvin ((f - 32)/1.8 + 273.15)
