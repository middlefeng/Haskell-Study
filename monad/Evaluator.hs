

{-# LANGUAGE Rank2Types #-}



module Evaluator where




data Term = Con Int | Div Term Term




eval :: Term -> Int

eval (Con a) = a
eval (Div t u) = (eval t) `div` (eval u)





data M a = Raise Exception | Return a
type Exception = String


eval' :: Term -> M Int
eval' (Con a) = Return a
eval' (Div t u) =
        case (eval' t) of
            Raise e -> Raise e
            Return a ->
                case (eval' u) of
                    Raise e -> Raise e
                    Return b ->
                        if b == 0 then Raise "divide by zero"
                                  else Return (a `div` b)




-- ----- Identity Monad
--------------------------------

data M' a = M' a


instance Functor M' where

    fmap f (M' a) = M' (f a)



instance Applicative M' where
    
    pure = M'

    (M' f) <*> (M' a) = M' (f a)

instance Monad M' where

    return = M'

    (M' a) >>= f = f a




-- ----- State Monad
--------------------------------


data StateM state a = StateM (state -> (a, state))


instance Functor (StateM state) where

    fmap f (StateM m) = (StateM nextState) where
                    nextState s = let (a, t) = (m s) in
                                    ((f a), t)


instance Applicative (StateM state) where
    
    pure a = StateM (\s -> (a, s))

    (StateM mf) <*> (StateM m) = (StateM nextState) where               -- m           :: state -> (a, state)
                                                                        -- nextState   :: state -> (b, state)
                                                                        -- mf          :: state -> (a -> b, state),
                            nextState s = let (a, t) = (m s) in         -- a :: a
                                                                        -- s :: state
                                                                        -- t :: state
                                            let (f, t') = (mf t) in     -- (mf t) :: (a -> b, state)
                                                ((f a), t')



instance Monad (StateM state) where
    
    (StateM m) >>= f = (StateM nextState) where
                            nextState s = let (a, t) = (m s) in             -- m :: state -> (a, state)
                                                                            -- a :: a
                                                                            -- t :: state
                                            let (StateM m') = (f a) in      -- f :: a -> StateM (state -> (b, state))
                                                                            -- m' :: (state -> (b, state))
                                                m' t


tick :: (Num state) => StateM state ()

tick = StateM (\x -> ((), x + 1))




showStatM :: (Show state, Show a) => (StateM state a) -> state -> String

showStatM (StateM f) initialState =  let result = (f initialState) in
                                        show result




eval'' :: Term -> (Monad m) => m Int

eval'' (Con a) = return a
eval'' (Div t u) = (eval'' t) >>=
                   \a -> (eval'' u) >>=
                   \b -> return (a `div` b)






evalTick :: Term -> StateM Int Int

evalTick (Con a) = return a
evalTick (Div t u) = (evalTick t) >>=
                     \a -> (evalTick u) >>=
                     \b -> tick >>
                     return (a `div` b)





