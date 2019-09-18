module ID3.V2p2
    ( parseTags
    , FrameHeader
    ) where

import Control.Monad (guard, when)
import Data.Char (isAsciiUpper, isDigit)
import Data.Bits (shiftL, Bits(..))
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ID3.Header
import ID3.Unsynchronisation (deunsynchronise)
import ParseBS

type FrameHeader = (String, Int)

bytesToInteger :: [Word8] -> Int
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = map w2c <$> count 3 headerChar
    where headerChar = satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

-- size excludes header size
parseFrameHeaderSize :: Parse Int
parseFrameHeaderSize = do
    size  <- bytesToInteger . L.unpack <$> parseBytes 3
    guard $ size > 0
    return size

parseFrameHeader :: Parse FrameHeader
parseFrameHeader = (,) <$> parseFrameHeaderID <*> parseFrameHeaderSize

parseFrame :: Parse FrameHeader
parseFrame = do
    header@(id, size) <- parseFrameHeader
    frameData <- parseBytes size
    return header

-- must have at least one frame
parseTags :: ID3Header -> Parse [FrameHeader]
parseTags tagHeader = do
    let size  = id3Size tagHeader
        flags = id3Flags tagHeader
        unsynchronisation = testBit flags 7
        compression       = testBit flags 6

    -- TODO: remaining bits should be 0
    guard (not compression)
    when unsynchronisation $ deunsynchronise size
    some parseFrame
