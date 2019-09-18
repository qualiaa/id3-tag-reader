module ID3.V2p3
    ( parseTag
    , FrameHeader
    ) where

import Control.Monad (guard, when)
import Data.Char (isAsciiUpper, isDigit)
import Data.Bits (countTrailingZeros, shiftL, Bits(..))
import Data.Digest.CRC32
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ID3.Header
import ID3.Unsynchronisation (deunsynchronise, parseUnsyncByte)
import ParseBS

type FrameHeader = (String, Int, [Word8])

bytesToInteger :: (Integral a, Bits a, Integral b, Bits b) => [b] -> a
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = count 4 headerChar
    where headerChar = w2c <$> satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

-- size excludes header size
parseFrameHeaderSize :: Parse Int
parseFrameHeaderSize = do
    size  <- bytesToInteger <$> count 4 parseByte
    guard $ size > 0
    return size

parseFrameHeader :: Parse FrameHeader
parseFrameHeader = (,,)
    <$> parseFrameHeaderID
    <*> parseFrameHeaderSize
    <*> count 2 parseByte

parseFrame :: Parse FrameHeader
parseFrame = do
    header@(id, size, flags) <- parseFrameHeader
    frameData <- parseBytes size
    return header

checkCRC :: Int -> Parse ()
checkCRC framesSize = do
    correctCRC <- bytesToInteger <$> count 4 parseUnsyncByte
    calculatedCRC <- crc32 . L.take framesSize' <$> look
    guard $ correctCRC == calculatedCRC

    where framesSize' = fromIntegral framesSize

parseExtendedHeader :: Int -> Parse ()
parseExtendedHeader tagSize = do
    -- tagSize must not include unsynchronisation bytes.
    --
    -- headerSize does not include itself.
    --
    -- Unclear if headerSize includes unsynchronisation bytes, but the
    -- specification says it will either be 6 or 10, in which case it must not
    -- consider unsynchronisation.
    --
    -- Padding is padding times 0x00 bytes at end of tag, so unsynchronisation
    -- shouldn't affect its length.
    --
    -- We need to compute CRC on all the frame bytes before synchronisation. So
    -- we can use tagSize and padding to work out how many bytes that is.
    -- Fingers crossed.
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

-- Must have at least one frame
parseTag :: ID3Header -> Parse String
parseTag tagHeader = do
    let tagSize  = id3Size tagHeader
        flags = id3Flags tagHeader
        unsynchronisation = testBit flags 7
        extendedHeader    = testBit flags 6
        experimental      = testBit flags 5

    -- Remaining bits should be 0
    guard $ not experimental && countTrailingZeros flags >= 5

    let ext = when extendedHeader . parseExtendedHeader
    if not unsynchronisation then ext tagSize
                             else do
                                newSize <- deunsynchronise tagSize
                                ext newSize
    show <$> some parseFrame
