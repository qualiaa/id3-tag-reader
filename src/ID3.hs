module ID3
    ( someFunc
    ) where

import Prelude hiding (fail)
import System.IO (openFile, IOMode(..))
import Data.Maybe
import Control.Applicative ((<|>))
--import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import ParseBS
import qualified ID3.V2p2
import ID3.Header

parseV1 :: Parse ()
parseV1 = fail "Not implemented"

parseV2 :: Parse ()
parseV2 = do
    header <- parseHeader
    case version header of
        (2,0) -> ID3.V2p2.parseTags 
        otherwise -> fail "Unhandled version"

parseID3 :: Parse ()
parseID3 = parseV2 <|> parseV1

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents

    print $ parse parseHeader fileContents
