module ID3.Unsynchronisation
    ( deunsynchronise
    , tagsWithoutUnsync
    , fileWithoutUnsync
    , unsyncByte
    ) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L

import ParseBS

unsyncByte :: Parse Word8
unsyncByte = head <$>  bytes [0xFF,0x00]

tagsWithoutUnsync :: Int -> Parse [Word8]
tagsWithoutUnsync tagsSize = replicateM tagsSize $ unsyncByte <|> parseByte

fileWithoutUnsync :: Int -> Parse L.ByteString
fileWithoutUnsync tagsSize = do
    tagBytes <- tagsWithoutUnsync tagsSize
    rest <- look
    return $ (L.pack tagBytes) <> rest

deunsynchronise :: Int -> Parse ()
deunsynchronise tagsSize = do
    remainingInput <- look
    let Just fixedInput = parse (fileWithoutUnsync tagsSize) remainingInput
    puts fixedInput
