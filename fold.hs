

foldl' _    zero []         = zero
foldl' step zero (x:xs)     =
    let new = step zero x in
        new `seq` (foldl' step new xs)

main = putStrLn (show (foldl' (+) 0 [1,2,3]))
