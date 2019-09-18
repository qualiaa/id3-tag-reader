module ID3
    ( someFunc
    ) where

import Prelude hiding (fail)
import System.IO (openFile, IOMode(..))
import Data.Maybe
import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.ByteString.Lazy as L

import ParseBS
import ID3.Header
import qualified ID3.V2p2
import qualified ID3.V2p3

parseV1 :: Parse ()
parseV1 = fail "Not implemented"

parseV2 :: Parse String
parseV2 = do
    header <- parseHeader
    case id3Version header of
        (2,_) -> ID3.V2p2.parseTag header
        (3,_) -> ID3.V2p3.parseTag header
        otherwise -> fail "Unhandled version"

parseID3 :: Parse String
parseID3 = parseV2 -- <|> parseV1

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents
    print $ parse parseID3 fileContents

    case parse parseHeader fileContents of
        Nothing -> return () --putStrLn $ file ++ ": Could not parse header"
        Just header -> when (flags /= 0 && vers == 2) $ putStrLn $ file ++ ": " ++ show flags
            where flags = id3Flags header
                  vers  = fst $ id3Version header
