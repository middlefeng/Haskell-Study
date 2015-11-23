
module Prettify
(
    Doc         ,
    empty       ,
    char        ,
    text        ,
    line        ,
    double      ,
    punctuate   ,
    hcat        ,
    fsep        ,
    compact     ,
    (<>)
)
where


import qualified Data.List (foldr)


data Doc =    Empty
            | Char Char
            | Text String
            | Line
            | Concat Doc Doc
            | Union Doc Doc
              deriving (Eq, Show) 



empty = Empty


(<>) :: Doc -> Doc -> Doc

(<>) a Empty    = a
(<>) Empty b    = b
(<>) a b        = Concat a b



(</>) :: Doc -> Doc -> Doc

(</>) x y = x <> softline <> y






char :: Char -> Doc

char c = Char c



text :: String -> Doc

text "" = Empty
text s  = Text s



line :: Doc

line = Line



softline :: Doc

softline = group line




double :: Double -> Doc

double d = Text (show d)



punctuate :: Doc -> [Doc] -> [Doc]

punctuate p (x:xs) = (x <> p) : punctuate p xs
punctuate p x@(_) = x



flatten :: Doc -> Doc

flatten Line            = Char ' '
flatten (Concat x y)    = (Concat (flatten x) (flatten y))
flatten (Union x _)     = flatten x
flatten others          = others    



group :: Doc -> Doc

group x = flatten x `Union` x



fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc

fold f = Data.List.foldr f empty



hcat :: [Doc] -> Doc

hcat = fold (<>) 



fsep :: [Doc] -> Doc

fsep = fold (</>) 





compact :: Doc -> String

compact x = transform [x] where
                transform []        = ""
                transform (d:ds)    = 
                    case d of
                        Empty       ->   ""
                        Char c      ->   c : transform ds
                        Text s      ->   s ++ transform ds
                        Line        ->   '\n' : transform ds
                        Concat a b  ->   transform (a:b:ds)
                        Union _ b   ->   transform (b:ds)




