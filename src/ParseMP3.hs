module ParseMP3
    ( someFunc
    ) where

import System.IO (openFile, IOMode(..))
--import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import ParseBS

parseID3 :: Parse
parseID3 = string "ID3"

doThing :: L.ByteString -> Maybe String
doThing = parse (

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents
    print $ doThing fileContents
