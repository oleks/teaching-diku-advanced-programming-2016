module MatchParser where

import Control.Applicative
  ( Alternative((<|>), empty, many, some) )

import Control.Monad ( void )

newtype MatchParser a = MatchParser {
  runParser :: String -> Maybe (a, String)
}

instance Functor MatchParser where
  fmap f m = m >>= \a -> return (f a)

instance Applicative MatchParser where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

instance Monad MatchParser where
  -- return :: a -> MatchParser a
  return a = MatchParser $ \ s -> Just (a, s)

  -- (>>=) :: MatchParser a
  --          -> (a -> MatchParser b)
  --          -> MatchParser b 
  m >>= f = MatchParser $ \s -> do
    (a, s') <- runParser m s
    runParser (f a) s'

  -- fail :: String -> MatchParser a
  fail _ = MatchParser $ \ _ -> Nothing

getc :: MatchParser Char
getc = MatchParser getc'
  where getc' "" = Nothing
        getc' (x:xs) = Just (x, xs)

reject :: MatchParser a
reject = MatchParser $ \ _ -> Nothing

char :: Char -> MatchParser Char
char c = do
  c' <- getc
  if c == c' then return c else reject

string :: String -> MatchParser String
string "" = return ""
string (c:cs) = do
  void $ char c
  void $ string cs
  return (c:cs)

chars :: [Char] -> MatchParser Char
chars cs = do
  c <- getc
  if c `elem` cs then return c else reject

wild :: MatchParser ()
wild = void $ getc

parse :: MatchParser a -> String -> Maybe (a, String)
parse = runParser

instance Alternative MatchParser where
  -- empty :: a -> MatchParser a
  empty = reject

  -- (<|>) :: MatchParser a -> MatchParser a
  --          -> MatchParser a
  p <|> q = MatchParser $ \cs ->
    parse p cs <|> parse q cs

satisfy :: (Char -> Bool) -> MatchParser Char
satisfy p = do
  c <- getc
  if p c then return c else reject

keywords :: [String]
keywords = ["if"]

varName :: MatchParser String
varName = do
  cs <- some $ chars ['a'..'z']
  if cs `elem` keywords then reject else return cs

ntimes :: Word -> MatchParser a -> MatchParser [a]
ntimes 0 _ = reject
ntimes 1 p =
  do v <- p; return [v]
ntimes n p = do
  v <- p
  vs <- ntimes (n-1) p
  return (v:vs)

lenString :: MatchParser String
lenString = do
  cn <- chars ['1'..'9']
  cns <- many $ chars ['0'..'9']
  let n = read (cn:cns) -- See report.pdf
  ntimes n (chars ['a'..'z'])

get :: MatchParser String
get = MatchParser $ \ s -> Just (s, s)

eol :: MatchParser ()
eol = do
  s <- get
  case s of
    ""  -> return ()
    _   -> reject
