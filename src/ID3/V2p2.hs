module ID3.V2p2
    ( parseTag
    , FrameHeader
    , UnparsedFrame
    , Frame(..)
    , parseTextFrame
    , parseComment
    , parsePIC
    , parseUFI
    , parseULT
    ) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.Bits (countTrailingZeros, shiftL, Bits(..))
import Data.Char (isAsciiUpper, isDigit)
import Data.Text.Encoding (decodeLatin1, decodeUtf16LEWith, decodeUtf16BEWith)
import Data.Text.Encoding.Error (replace)
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

import ID3.Header
import ID3.Unsynchronisation (deunsynchronise)
import ParseBS

type FrameHeader = (String, Int)
type UnparsedFrame = (FrameHeader, L.ByteString)
type LanguageCode = String
data Frame = TextFrame T.Text
           | CommentFrame LanguageCode T.Text T.Text
           | UniqueFileIdentifierFrame String L.ByteString
           | PictureFrame String Word8 T.Text L.ByteString
           | UnsyncLyricsFrame LanguageCode T.Text T.Text
           deriving Show

bsToInteger :: L.ByteString -> Int
bsToInteger = bytesToInteger . L.unpack

bytesToInteger :: [Word8] -> Int
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = count 3 headerChar
    where headerChar = w2c <$> satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

-- size excludes header size
parseFrameHeaderSize :: Parse Int
parseFrameHeaderSize = do
    size  <- bsToInteger <$> parseBytes 3
    guard $ size > 0
    return size

parseFrameHeader :: Parse FrameHeader
parseFrameHeader = (,) <$> parseFrameHeaderID <*> parseFrameHeaderSize

extractFrame :: Parse UnparsedFrame
extractFrame = do
    header@(_, size) <- parseFrameHeader
    frameData <- parseBytes size
    return (header, frameData)

parsePadding :: Int -> Parse ()
parsePadding n = bytes (replicate n 0) >> return ()

-- Remove the remaining tag after the header. The remaining parse state should
-- contain only MPEG data after this function
parseTag :: ID3Header -> Parse [UnparsedFrame]
parseTag tagHeader = do
    let size  = id3Size tagHeader
        flags = id3Flags tagHeader
        unsynchronisation = testBit flags 7
        compression       = testBit flags 6

    -- Compression algorithm is not defined by the standard so we cannot unparse
    -- compressed tags
    guard $ not compression
    -- All but first two flag bits should be zero
    guard $ countTrailingZeros flags >= 6

    -- Remove desynchronisation if present - this removes bytes from the input
    newSize <- if unsynchronisation then deunsynchronise size
                                    else return size


    p1 <- L.length <$> look
    -- Extract list of one or more frames
    frames <- some extractFrame
    p2 <- L.length <$> look

    -- If the number of frame bytes parsed is less than the tag size, either
    -- the tag has 0 padding, we've failed to parse a valid frame, or the tag is
    -- invalid.
    let numPaddingBytes = newSize - fromIntegral (p1 - p2)

    -- Ensure all padding bytes are 0 - otherwise we've failed to parse something
    parsePadding numPaddingBytes
    return frames


{- ------------------------------------------------------------------- -
 - ID3 v2.2 Frames
 - ------------------------------------------------------------------- -}

{-
 - BUF
 - CNT
 - COM
 - CRA
 - CRM
 - ETC
 - EQU
 - GEO
 - IPL
 - LNK
 - MCI
 - MLL
 - PIC
 - POP
 - REV
 - RVA
 - SLT
 - STC
 - UFI
 - ULT

 - URL frames
 -
 - WAF
 - WAR
 - WAS
 - WCM
 - WCP
 - WPB

 - Text frames
 -
 - TT1      Content group description             e.g.piano - concerto/weather-hurricane
 - TT2      Title/Songname/Content description
 - TT3      Subtitle/Description refinement       e.g. op16/performed live at wembley
 - TP1      Lead artist/performer/soloist/group   separated by / character
 - TP2      Band/Orchestra/Accompaniment
 - TP3      Conductor
 - TP4      Interpreted/remixed/modifed by
 - TCM      Composer(s)                           separated by / character
 - TXT      Lyricist/text writer
 - TLA      Language(s)                           3 byte ISO-639-2 codes. Multiple languages are listed in order of appearance
 - TCO      Content type                          (Numeric string. Refs to ID3v1 content type are wrapped in "(" ")".
 -                                                Optionally followed by refinement e.g. (4)Eurodisco. Can mix several e.g. (51)(39).
 -                                                If refinement should begin with (, starts with (( instead.
 - TAL      Album/Movie/Show title
 - TPA      Part of a set                         Numeric string optionally extended with "/". e.g. 1/2
 - TRK      Track number                          Numeric string optionally extended with "/". e.g. 4/9
 - TRC      ISRC                                  International Standard Recoding Code
 - TYE      Year                                  4 character numeric string
 - TDA      Date                                  4 character numeric string DDMM
 - TIM      Time                                  4 character numeric string HHMM
 - TRD      Recording dates                       e.g. "4th-7th June, 12 June"
 - TMT      Media Type                            Textstring or reference to predefined types in "(" ")" similar to TCO
 - TFT      File type                             MPG /1/2/3/2.5/AAC or other type
 - TBP      BPM                                   Integral numeric string
 - TCR      Copyright                             year and space (5 characters) followed by copyright of original piece of music
 -                                                Must be displayed as "Copyright "(C)" "...
 - TPB      Publisher
 - TEN      Encoded by
 - TSS      Software/hardware settings
 - TOF      Original Filename
 - TLE      Length                                Numeric string in ms
 - TSI      Size                                  Numeric string in bytes excl. tag
 - TDY      Playlist delay                        Numeric string in ms (silence between every song in a playlist)
 - TKE      Initial key                           String with max len 3: [A-G][b#]?[mo]?
 - TOT      Original album/movie/show
 - TOA      Original artist(s)/performer(s)       Separated with /
 - TOL      Original lyricist(s)
 - TOR      Original release year                 As TDY
 -
 - TXX      User defined...   "TXX"
 -          Frame size        $xx xx xx
 -          Text encoding     $xx
 -          Description       <textstring> $00 (00)
 -          Value             <textstring>
 -}

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

takeEveryOther [] = []
takeEveryOther [x] = [x]
takeEveryOther (x:_:xs) = x : takeEveryOther xs

parseTextFrame :: FrameHeader -> Parse Frame
parseTextFrame (id, size) = do
    encoding <- parseEncoding
    str <- L.toStrict <$> parseBytes (size - 1)
    guard $ encoding == Latin1 || even (S.length str)
    return . TextFrame . decodeText encoding . fst $ zeroTerminate encoding str

parseComment :: FrameHeader -> Parse Frame
parseComment (id, size) = do
    encoding <- parseEncoding
    lang <- parseString 3
    str <- L.toStrict <$> parseBytes (size - 4)

    let (description, rest) = zeroTerminate encoding str
        text = fst $ zeroTerminate encoding rest

    return $ CommentFrame lang (decodeText encoding description) (decodeText encoding text)

parseUFI :: FrameHeader -> Parse Frame
parseUFI (id, size) = do
    owner <- manyTill parseChar (byte 0)
    let left = size - (length owner + 1)
    UniqueFileIdentifierFrame <$> return owner <*> parseBytes left

parseZeroString :: Encoding -> Parse T.Text

parseZeroString Latin1 = toText <$> manyTill parseByte (byte 0)
    where toText = decodeText Latin1 . S.pack

parseZeroString Utf16 = do
    endianness <- (bytes [0xFF, 0xFE]            >> return LE)
              <|> (optional (bytes [0xFE, 0xFF]) >> return BE)
    toText endianness <$> manyTill (parseBytes 2) (bytes [0,0])

    where toText e = decode e . S.concat . map (L.toStrict)

            --bytes [0,0] >> return [] <|> (:) <$> parseBytes 2 <*> parseZeroString'


parsePIC :: FrameHeader -> Parse Frame
parsePIC (id, size) = do
    encoding <- parseEncoding
    fmt <- parseString 3
    picType <- parseByte
    p1 <- L.length <$> look
    desc <- parseZeroString encoding
    p2 <- L.length <$> look
    let left = size - 5 - fromIntegral (p1 - p2)
    pic <- parseBytes left
    return $ PictureFrame fmt picType desc pic

parseULT :: FrameHeader -> Parse Frame
parseULT (id, size) = do
    encoding <- parseEncoding
    UnsyncLyricsFrame
        <$> parseString 3
        <*> parseZeroString encoding
        <*> parseZeroString encoding
