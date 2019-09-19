module ID3.Unsynchronisation
    ( deunsynchronise

    , tagWithoutUnsync
    , fileWithoutUnsync

    , parseUnsyncByte
    ) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ParseBS

unsyncByte :: Parse Word8
unsyncByte = head <$>  bytes [0xFF,0x00]

parseUnsyncByte :: Parse Word8
parseUnsyncByte = unsyncByte <|> parseByte

tagWithoutUnsync :: Int -> Parse [Word8]
tagWithoutUnsync 0 = return []
tagWithoutUnsync tagSize = do
    (:) <$> unsyncByte <*> tagWithoutUnsync (tagSize-2) <|>
        (:) <$> parseByte <*> tagWithoutUnsync (tagSize-1)

fileWithoutUnsync :: Int -> Parse L.ByteString
fileWithoutUnsync tagSize = do
    tagBytes <- tagWithoutUnsync tagSize
    rest <- look
    return $ (L.pack tagBytes) <> rest

deunsynchronise :: Int -> Parse Int
deunsynchronise tagSize = do
    remainingInput <- look
    let Just fixedInput = parse (fileWithoutUnsync tagSize) remainingInput
    puts fixedInput
    let oldSize = fromIntegral $ L.length remainingInput
        newSize = fromIntegral $ L.length fixedInput
        newTagSize = tagSize + newSize - oldSize
    return $ fromIntegral newTagSize
