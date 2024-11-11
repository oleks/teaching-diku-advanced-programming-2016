newtype Reader d a = Reader { runRd :: d -> a }

-- Functor, Applicative, ...

instance Functor (Reader d) where
  fmap f m = m >>= \a -> return (f a)

instance Applicative (Reader d) where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

-- Monad

instance Monad (Reader d) where
  return a = Reader $ \ _ -> a
  m >>= f = Reader $ \ d ->
    let a = runRd m d
    in runRd (f a) d

getData :: Reader d d
getData = Reader $ \ d -> d

withData :: d -> Reader d a -> Reader d a
withData d m = Reader $ \ _ -> runRd m d


data Expr = Con Int | Add Expr Expr
          | Var String | Let String Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Reader (String -> Int) Int
eval (Con v) = return v
eval (Var n) =
  do d <- getData; return $ d n
eval (Add e1 e2) =
  do v1 <- eval e1; v2 <- eval e2; return $ v1 + v2
eval (Let x ex e2) = do
  vx <- eval ex; d <- getData;
  withData (\ y -> if y == x
                   then vx else d y) (eval e2)

run :: Expr -> Int
run e = runRd (eval e) (\x -> error $ "unbound: " ++ x)
