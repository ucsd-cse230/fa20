{-# LANGUAGE FlexibleInstances #-}

module Lec_11_17_20 where

-- import Prelude hiding (return, (>>=))

inc :: Int -> Int 
inc x = x + 1

data Expr
  = Number Int            -- ^ 0,1,2,3,4
  | Plus   Expr Expr      -- ^ e1 + e2
  | Minus  Expr Expr      -- ^ e1 - e2
  | Mult   Expr Expr      -- ^ e1 * e2
  | Div    Expr Expr      -- ^ e1 / e2
  deriving (Show)

eval :: Expr -> Int
eval (Number v)    = v
eval (Plus e1 e2)  = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Mult e1 e2)  = eval e1 * eval e2
eval (Div e1 e2)   = eval e1 `div` eval e2

data Result v 
  = Ok v                -- ^ v is a valid "result"
  | Error String        -- ^ oops an error occurred with message String
  deriving (Eq, Show) 


instance Monad Result where
  -- return  :: a -> Result a
  return v = Ok v

  -- (>>=) :: Result a -> (a -> Result b) -> Result b
  e >>= doStuff = case e of
                    Error msg -> Error msg
                    Ok v      -> doStuff v

{- 
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a
-}


foo :: Monad m => m a -> m b -> m (a, b)
foo m1 m2 = m1 >>= (\x1 -> 
            m2 >>= (\x2 -> 
            return (x1, x2)
            )) 

foo' m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (m1, m2)


safeEval :: Expr -> Result Int
safeEval (Number n)    = Ok n
safeEval (Plus  e1 e2) = do { v1 <- safeEval e1;
                              v2 <- safeEval e2;
                              return (v1 + v2)
                            }
safeEval (Minus e1 e2) = do { v1 <- safeEval e1;
                              v2 <- safeEval e2;
                              return (v1 - v2)
                            }
safeEval (Mult e1 e2) = do { v1 <- safeEval e1;
                             v2 <- safeEval e2;
                             return (v1 * v2)
                           } 
safeEval (Div e1 e2)   = do { v1 <- safeEval e1; v2 <- safeEval e2; 
                              if v2 == 0 
                                then (Error ("Oops dbz in: " ++ show e2))
                                else (Ok (v1 `div` v2))
                            }  

instance Applicative Result where
instance Functor Result where

