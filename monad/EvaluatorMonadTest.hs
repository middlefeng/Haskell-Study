

module EvaluatorMonadTest where


import Evaluator as E


testApplicativeIdentity ::  (Eq a, Eq state) =>
                            a ->        -- inner for the Applictive
                            state ->    -- test parameter for the resultant Applicative
                            Bool

testApplicativeIdentity a s = let m = (\state -> (a, state)) in
                                    let (E.StateM m') = (pure id <*> (E.StateM m)) in
                                        let (b', t') = m' s
                                            (b, t) = m s in
                                                (b' == b) && (t' == t)



testApplicativeHomo ::  (Int -> Int) ->         -- inner for the first Applicatives map function
                        Int ->                  -- inner for the second Applicatives
                        Int ->                  -- test parameter for the resultant two Applicatives
                        Bool 

testApplicativeHomo f s x = let mf :: E.StateM Int (Int -> Int)
                                mf = pure f
                                mx :: E.StateM Int Int
                                mx = pure s in
                                    let (E.StateM m) = mf <*> mx
                                        (E.StateM m') = pure (f s) in
                                        let (b1, t1) = m x
                                            (b2, t2) = m' x in
                                                (b1 == b2) && (t1 == t2)





testMonadHomo :: (Int -> E.StateM Int Int) ->     -- second parameter to >>=
                 Int ->                           -- inner for the beginning Monad
                 Int ->                           -- test parameter for the resultant two Monads
                 Bool 


testMonadHomo f s x = let m0 :: E.StateM Int Int
                          m0 = f s
                          m1 :: E.StateM Int Int
                          m1 = ((return s) >>= f) in
                                let (E.StateM f0) = m0
                                    (E.StateM f1) = m1 in
                                        let (b1, t1) = f0 x
                                            (b2, t2) = f1 x in
                                                (b1 == b2) && (t1 == t2)



                                                


