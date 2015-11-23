

module ParamPoly where


data TypeFirst a = TypeFirst [a]




createTypeFirst :: a -> TypeFirst a

createTypeFirst a = TypeFirst [a]



showTypeFirst :: TypeFirst a -> [a]

showTypeFirst (TypeFirst a) = a

