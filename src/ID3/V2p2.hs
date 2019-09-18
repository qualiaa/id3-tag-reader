module ID3.V2p2
    ( parseTag
    , FrameHeader
    ) where

import Control.Monad (guard, when, void)
import Data.Char (isAsciiUpper, isDigit)
import Data.Bits (countTrailingZeros, shiftL, Bits(..))
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ID3.Header
import ID3.Unsynchronisation (deunsynchronise)
import ParseBS

type FrameHeader = (String, Int)

bytesToInteger :: [Word8] -> Int
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = count 3 headerChar
    where headerChar = w2c <$> satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

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

-- Must have at least one frame
parseTag :: ID3Header -> Parse String
parseTag tagHeader = do
    let size  = id3Size tagHeader
        flags = id3Flags tagHeader
        unsynchronisation = testBit flags 7
        compression       = testBit flags 6

    -- Remaining bits should be 0
    guard $ not compression && countTrailingZeros flags >= 6
    when unsynchronisation $ void $ deunsynchronise size
    show <$> some parseFrame
