module ParseBS
  ( Parse(..)
  , parse

  , peekByte
  , look
  , (+++)

  , satisfy
  , count
  , between
  , choice
  , option
  , optional

  , munch
  , munch1
  , many
  , some
  , skipMany
  , skipMany1
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , manyTill

  , string
  , char
  , byte
  , bytes
  , eof
  , skipZeros
  , skipSpaces

  , parseByte
  , parseBytes
  , parseChar
  , parseString

  , fail
  , w2c
  , puts
  ) where

import Prelude hiding (fail)
import Data.Bifunctor (second)
import Data.Char (chr, isSpace)
import Data.List (foldl1')
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Word (Word8)
import Control.Monad.Fail as F (MonadFail(fail))
import Control.Applicative (Alternative(..))

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState { getString :: L.ByteString }
newtype Parse a = Parse { runParse :: ParseState -> Maybe (a, ParseState) }

parse :: Parse a -> L.ByteString -> Maybe a
parse parser = fmap fst . runParse parser . ParseState

instance Functor Parse where
    fmap f parser = parser >>= (\a -> pure (f a))

instance Applicative Parse where
    pure a = Parse (\s -> Just (a, s))
    pf <*> pa = do
        f <- pf
        f <$> pa

instance Alternative Parse where
    empty = Parse (\_ -> Nothing)
    Parse l <|> Parse r = Parse (\s -> (l s) <|> (r s))
    --many p = ((:) <$> p <*> (many p)) <|> pure []
                    

instance Monad Parse where
    return = pure
    pa >>= f = Parse $ \s -> runParse pa s >>= (\(a, s') -> runParse (f a) s')

instance MonadFail Parse where
    fail _ = empty

-- State functions
get :: Parse ParseState
get = Parse (\s -> Just (s, s))
gets = getString <$> get

put :: ParseState -> Parse ()
put s = Parse (\_ -> Just ((), s))
puts = put . ParseState 

-- Basic functions
parseByte :: Parse Word8
parseByte = Parse (\s -> second ParseState <$> (L.uncons $ getString s))

look :: Parse L.ByteString
look = gets

-- This should be symmetric choice but for now it's left-biased choice because I
-- don't understand anything well enough to implement symmetric choice
(+++) = (<|>)

-- Byte functions
peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons <$> gets

parseBytes :: Int -> Parse L.ByteString
parseBytes n = do
    (parsed, remaining) <- L.splitAt (fromIntegral n) <$> gets
    if L.length parsed /= (fromIntegral n)
        then fail "End of input"
        else puts remaining >> return parsed

satisfy :: (Word8 -> Bool) -> Parse Word8
satisfy p = do
    b <- parseByte
    if p b then return b else fail "Condition not satisfied"

munch :: (Word8 -> Bool) -> Parse L.ByteString
munch p = do
    (parsed, remaining) <- L.span p <$> look
    puts remaining
    return parsed

munch1 :: (Word8 -> Bool) -> Parse L.ByteString
munch1 p = do
    b <- parseByte
    if p b then return . L.cons b =<< munch p
           else fail $ "Input does not match character: " ++ [w2c b]

skipZeros :: Parse ()
skipZeros = munch (== 0) >> return ()

-- Char functions
parseChar :: Parse Char
parseChar = w2c <$> parseByte

parseString :: Int -> Parse String
parseString = fmap L8.unpack . parseBytes

skipSpaces :: Parse ()
skipSpaces = munch (isSpace . w2c) >> return ()

-- Matchers
string :: String -> Parse String
string s = do
    s' <- parseString (length s)
    if s == s' then return s else fail $ "Input does not match string: " ++ s

char :: Char -> Parse Char
char c = w2c <$> satisfy ((c ==) . w2c)

byte :: Word8 -> Parse Word8
byte b = satisfy (b==)

bytes :: [Word8] -> Parse [Word8]
bytes = sequence . map byte

eof :: Parse ()
eof = do
    next <- peekByte
    if next & isNothing then return () else fail "Not EOF"

-- Combinators
count :: Int -> Parse a -> Parse [a]
count n p = sequence $ replicate n p

between :: Parse open -> Parse close -> Parse a -> Parse a
between open close p = open *> p <* close

choice :: [Parse a] -> Parse a
choice [] = fail "Empty choice"
choice xs = foldl1' (+++) xs

option :: a -> Parse a -> Parse a
option a p = p <|> return a

optional :: Parse a -> Parse ()
optional p = (p >> return ()) +++ return ()

skipMany  :: Parse a -> Parse ()
skipMany1 :: Parse a -> Parse ()
skipMany  p = many p >> return ()
skipMany1 p = some p >> return ()

sepBy  :: Parse a -> Parse sep -> Parse [a]
sepBy1 :: Parse a -> Parse sep -> Parse [a]
sepBy  p sep = sepBy1 p sep +++ return []
sepBy1 p sep = (:) <$> p <*> many (sep >> p)

endBy  :: Parse a -> Parse end -> Parse [a]
endBy1 :: Parse a -> Parse end -> Parse [a]
endBy  p sep = endBy1 p sep +++ return []
endBy1 p sep = sepBy1 p sep <* sep

{- Don't really get this one
chainl  :: Parse a -> Parse (a -> a -> a) -> a -> Parse a
chainr  :: Parse a -> Parse (a -> a -> a) -> a -> Parse a
chainl1 :: Parse a -> Parse (a -> a -> a)      -> Parse a
chainr1 :: Parse a -> Parse (a -> a -> a)      -> Parse a

chainl p op x = ?????
-}

manyTill :: Parse a -> Parse end -> Parse [a]
manyTill p end = (end >> return []) <|> ((:) <$> p <*> (manyTill p end))

w2c = chr . fromIntegral
