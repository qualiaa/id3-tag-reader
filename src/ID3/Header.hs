module ID3.Header
    ( parseHeader
    , ID3Header(..)
    , Version
    ) where

import Data.Bits (shiftL)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ParseBS

type Version = (Int, Int)

-- size excludes header size
data ID3Header = ID3Header
    { id3Version :: Version
    , id3Flags :: Word8
    , id3Size :: Int
    } deriving Show

parseVersion :: Parse Version
parseVersion = (,) <$> component <*> component
    where component = fromIntegral <$> parseByte

-- size must be 4x 0xxxxxxx i.e. every MSB is 0, 28bits total
parseSize :: Parse Int
parseSize = do
    sizeBytes <- map fromIntegral . L.unpack <$> parseBytes 4
    return . sum . map (uncurry shiftL) $ zip sizeBytes [3*7,2*7..]

parseHeader :: Parse ID3Header
parseHeader = do
    string "ID3"
    ID3Header <$> parseVersion <*> parseByte <*> parseSize
