module ID3.V2p2
    ( parseTags
    ) where

import Data.Char (isAsciiUpper, isDigit)
import Data.Bits (shiftL, Bits(..))
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ID3.Header
import ParseBS

bytesToInteger :: [Word8] -> Int
bytesToInteger bytes = sum . map (uncurry shiftL) $ zip (map fromIntegral $ reverse bytes) [0,8..]

parseFrameHeaderID :: Parse String
parseFrameHeaderID = map w2c <$> count 3 headerChar
    where headerChar = satisfy (((||) <$> isAsciiUpper <*> isDigit) . w2c)

parseFrameHeaderSize :: Parse Int
parseFrameHeaderSize = do
    size  <- bytesToInteger . L.unpack <$> parseBytes 3
    return size

-- size excludes header size
-- must have at least one frame
parseFrameHeader :: Parse [Int]--(String, Int, L.ByteString)
parseFrameHeader = do
    id <- parseFrameHeaderID
    size <- map fromIntegral . L.unpack <$> parseBytes 3
    --frameData <- parseBytes size
    return size ---(id, size, frameData)

parseTags :: Parse ()
parseTags = return ()

