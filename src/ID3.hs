module ID3
    ( someFunc
    ) where

import Prelude hiding (fail)
import System.IO (openFile, IOMode(..))
import Data.Maybe
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as L

import ParseBS
import ID3.Header
import qualified ID3.V2p2

parseV1 :: Parse ()
parseV1 = fail "Not implemented"

parseV2 :: Parse [ID3.V2p2.FrameHeader]
parseV2 = do
    header <- parseHeader
    case id3Version header of
        (2,0) -> ID3.V2p2.parseTags header
        otherwise -> fail "Unhandled version"

parseID3 :: Parse [ID3.V2p2.FrameHeader]
parseID3 = parseV2 -- <|> parseV1

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents

    print $ parse parseID3 fileContents
