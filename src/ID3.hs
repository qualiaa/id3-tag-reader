module ID3
    ( someFunc
    ) where

import Prelude hiding (fail)
import System.IO (openFile, IOMode(..))
import Data.List (partition)
import Data.Maybe
import Control.Applicative ((<|>))
import Control.Monad (when, forM_)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.IO as TIO

import ParseBS
import ID3.Header
import qualified ID3.V1
import qualified ID3.V2p2
import qualified ID3.V2p3

parseID3 :: Parse [ID3.V2p2.UnparsedFrame]
parseID3 = parseV2-- <|> parseV1

parseV1 :: Parse String
parseV1 = ID3.V1.parseTag

parseV2 :: Parse [ID3.V2p2.UnparsedFrame]
parseV2 = do
    header <- parseHeader
    case id3Version header of
        (2,_) -> ID3.V2p2.parseTag header
        --(3,_) -> ID3.V2p3.parseTag header
        otherwise -> return []


handleFrames :: [ID3.V2p2.UnparsedFrame] -> IO ()
handleFrames frames = do
    let (textFrames, nontext) = partition (('T'==).head.fst.fst) frames
        nontextIDs = map (fst.fst) nontext

    putStrLn $ "Unhandled frames:" ++ show nontextIDs

    forM_ textFrames (\(header, bytes) -> do
        putStr $ fst header ++ ": "
        case parse (ID3.V2p2.parseTextFrame header) bytes of
            Nothing -> putStrLn "Failed to parse"
            Just result -> TIO.putStrLn result
        )

incrementalParse :: L.ByteString -> IO ()
incrementalParse input = do
    case try parseHeader of
        Nothing -> putStrLn "Could not parse header"
        Just header -> do
            let flags = id3Flags header
                version = id3Version header

            putStr $ "Version " ++ show version ++ ", "
            putStr $ "Flags: " ++ show flags ++ ". "

            case try parseID3 of
                Nothing -> putStrLn "Could not parse rest of file"
                Just [] -> putStrLn "Unhandled version"
                Just frames -> putStrLn "" >> handleFrames frames
    where try x = parse x input

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents

    putStr $ file ++ ": "

    incrementalParse fileContents
