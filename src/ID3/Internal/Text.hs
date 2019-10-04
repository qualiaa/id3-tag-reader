module ID3.Internal.Text
    ( parseEncoding
    , parseZeroString
    , zeroTerminate
    , decodeText
    , Encoding(..)
    ) where

import Control.Arrow (first, second)
import Control.Applicative ((<|>))
import Data.Text.Encoding (decodeLatin1, decodeUtf16LEWith, decodeUtf16BEWith)
import Data.Text.Encoding.Error (replace)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import ParseBS

data Endianness = BE | LE deriving Eq
data Encoding = Latin1 | Utf16 deriving Eq
parseEncoding = toEncoding <$> satisfy (`elem` [0,1])
    where toEncoding 0 = Latin1
          toEncoding 1 = Utf16

decode LE = decodeUtf16LEWith (replace '?')
decode BE = decodeUtf16BEWith (replace '?')

decodeUtf string
    | firstChar == le = decode LE rest
    | firstChar == be = decode BE rest
    | otherwise       = decode BE string
    where (firstChar, rest) = S.splitAt 2 string
          le = S.pack [0xff, 0xfe]
          be = S.pack [0xfe, 0xff]

-- parseFrameContent :: FrameHeader
decodeText :: Encoding -> (S.ByteString -> T.Text)
decodeText Latin1 = decodeLatin1
decodeText Utf16  = decodeUtf

zeroTerminate :: Encoding -> S.ByteString -> (S.ByteString, S.ByteString)
zeroTerminate Latin1 = second (S.drop 1) . S.break (==0)
zeroTerminate Utf16 = first S.concat . zeroTerminateUtf16

zeroTerminateUtf16 :: S.ByteString -> ([S.ByteString], S.ByteString)
zeroTerminateUtf16 s
    | S.length s < 2 = ([S.empty], S.empty)
    | char == zero   = ([S.empty], rest)
    | otherwise      = first (char :) $ zeroTerminateUtf16 rest
    where (char, rest) = S.splitAt 2 s
          zero = S.pack [0,0]

parseZeroString :: Encoding -> Parse T.Text

parseZeroString Latin1 = toText <$> manyTill parseByte (byte 0)
    where toText = decodeText Latin1 . S.pack

parseZeroString Utf16 = do
    endianness <- (bytes [0xFF, 0xFE]            >> return LE)
              <|> (optional (bytes [0xFE, 0xFF]) >> return BE)
    toText endianness <$> manyTill (parseBytes 2) (bytes [0,0])

    where toText e = decode e . S.concat . map (L.toStrict)

            --bytes [0,0] >> return [] <|> (:) <$> parseBytes 2 <*> parseZeroString'
