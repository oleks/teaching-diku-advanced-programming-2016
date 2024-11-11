newtype State s a = State { runSt :: s -> (a, s) }

-- Functor, Applicative, ...

instance Functor (State s) where
  fmap f m = m >>= \a -> return (f a)

instance Applicative (State s) where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

-- Monad

instance Monad (State s) where
  return a = State $ \ s -> (a, s)
  m >>= f = State $ \ s ->
    let (a, s1) = runSt m s
        (b, s2) = runSt (f a) s1
    in (b, s2)

get :: State s s
get = State $ \ s -> (s, s)

set :: s -> State s ()
set s = State $ \ _ -> ((), s)

with :: s -> State s a -> State s a
with s m = State $ \ _ -> runSt m s


data Expr = Con Int | Add Expr Expr
          | Var String | Let String Expr Expr
  deriving (Eq, Show)

eval :: Expr -> State (String -> Int) Int
eval (Con v) = return v
eval (Var n) =
  do s <- get; return $ s n
eval (Add e1 e2) =
  do v1 <- eval e1; v2 <- eval e2; return $ v1 + v2
eval (Let x ex e2) = do
  vx <- eval ex; s <- get;
  set (\ y -> if y == x then vx else s y)
  eval e2

run :: Expr -> (Int, String -> Int)
run e = runSt (eval e) (\x -> error $ "unbound: " ++ x)
