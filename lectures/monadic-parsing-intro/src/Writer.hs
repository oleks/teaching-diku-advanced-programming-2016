newtype Writer s a = Writer { runWr :: (a, s) }

-- Functor, Applicative, ...

instance Monoid s => Functor (Writer s) where
  fmap f m = m >>= \a -> return (f a)

instance Monoid s => Applicative (Writer s) where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

-- Monad

instance Monoid s => Monad (Writer s) where
  return a = Writer $ (a, mempty)
  m >>= f = Writer $
    let (a, s1) = runWr m
        (b, s2) = runWr (f a)
    in (b, s1 `mappend` s2)

-- no data to get, so just write:
write :: s -> Writer s ()
write s = Writer ((), s)

data Ops = OpAdd | OpMul
  deriving (Eq, Show)
data Expr = Con Int | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Writer [Ops] Int
eval (Con v) = return v
eval (Add e1 e2) = do
  v1 <- eval e1; v2 <- eval e2
  write [OpAdd]; return $ v1 + v2
eval (Mul e1 e2) = do
  v1 <- eval e1; v2 <- eval e2
  write [OpMul]; return $ v1 * v2

run :: Expr -> (Int, [Ops])
run e = runWr (eval e)
