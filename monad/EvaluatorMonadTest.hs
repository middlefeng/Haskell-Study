

module EvaluatorMonadTest where


import Evaluator as E


testApplicativeIdentity :: (Eq a, Eq state) => a -> state -> Bool

testApplicativeIdentity a s = let m = (\state -> (a, state)) in
                                    let (E.StateM m') = (pure id <*> (E.StateM m)) in
                                        let (b', t') = m' s
                                            (b, t) = m s in
                                                (b' == b) && (t' == t)



testApplicativeHomo :: (Int -> Int) -> Int -> Int -> Bool

testApplicativeHomo f s x = let mf :: E.StateM Int (Int -> Int)
                                mf = pure f
                                mx :: E.StateM Int Int
                                mx = pure s in
                                    let (E.StateM m) = mf <*> mx
                                        (E.StateM m') = pure (f s) in
                                        let (b1, t1) = m x
                                            (b2, t2) = m' x in
                                                (b1 == b2) && (t1 == t2)


                                                

