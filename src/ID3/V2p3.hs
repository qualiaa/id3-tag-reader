module ID3.V2p3
    ( parseTag
    , FrameHeader
    , UnparsedFrame
    ) where

import Control.Monad (guard, when)
import Data.Char (isAsciiUpper, isDigit)
import Data.Bits (countTrailingZeros, shiftL, Bits(..))
import Data.Digest.CRC32
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ID3.Header
import ID3.Unsynchronisation (deunsynchronise)
import ParseBS

type FrameHeader = (String, Int, [Word8])
type UnparsedFrame = (FrameHeader, L.ByteString)

bytesToInteger :: (Integral a, Bits a, Integral b, Bits b) => [b] -> a
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = count 4 headerChar
    where headerChar = w2c <$> satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

-- size excludes header size
parseFrameHeaderSize :: Parse Int
parseFrameHeaderSize = do
    size  <- bytesToInteger <$> count 4 parseByte
    --guard $ size > 0
    return size

parseFrameHeader :: Parse FrameHeader
parseFrameHeader = (,,)
    <$> parseFrameHeaderID
    <*> parseFrameHeaderSize
    <*> count 2 parseByte

extractFrame :: Parse UnparsedFrame
extractFrame = do
    header@(id, size, flags) <- parseFrameHeader
    frameData <- parseBytes size
    return (header, frameData)

checkCRC :: Int -> Parse ()
checkCRC framesSize = do
    correctCRC <- bytesToInteger <$> count 4 parseByte
    calculatedCRC <- crc32 . L.take framesSize' <$> look
    guard $ correctCRC == calculatedCRC

    where framesSize' = fromIntegral framesSize

parseExtendedHeader :: Int -> Parse Int
parseExtendedHeader tagSize = do
    -- tagSize must not include unsynchronisation bytes.
    --
    -- headerSize does not include itself. Describes deunsynchronised size.
    -- So header size is 6 if no CRC flag, or 10 if CRC flag is set
    headerSize       <- bytesToInteger <$> count 4 parseByte
    [flagsA, flagsB] <-                    count 2 parseByte
    padding          <- bytesToInteger <$> count 4 parseByte

    let crc = testBit flagsA 7
        framesSize = tagSize - (headerSize + 4) - padding

    -- Only first bit may be set
    guard $ flagsB == 0 && if crc
        then headerSize == 10 && countTrailingZeros flagsA == 7
        else headerSize == 6  && flagsA == 0

    when crc $ checkCRC framesSize
    return headerSize

parsePadding :: Int -> Parse ()
parsePadding n = bytes (replicate n 0) >> return ()

-- Must have at least one frame
parseTag :: ID3Header -> Parse [UnparsedFrame]
parseTag tagHeader = do
    let tagSize  = id3Size tagHeader
        flags = id3Flags tagHeader
        unsynchronisation = testBit flags 7
        extendedHeader    = testBit flags 6
        experimental      = testBit flags 5

    -- Do not attempt to parse experimental frames
    -- Remaining bits should be 0
    guard $ not experimental && countTrailingZeros flags >= 5

    -- Deunsynchronise data if necessary
    newSize <- if unsynchronisation then deunsynchronise tagSize
                                    else return tagSize

    -- Parse extended header
    newSize' <- if extendedHeader then parseExtendedHeader newSize
                                  else return newSize

    -- Extract at least one frame
    (frames, numBytes) <- parseWithSize (some extractFrame)

    parsePadding (newSize' - numBytes)

    return frames
