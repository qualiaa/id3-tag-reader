module ID3
    ( someFunc
    ) where

import Prelude hiding (fail)
import System.IO (openFile, IOMode(..))
import Data.List (partition)
import Data.Maybe
import Data.Either
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


frameParserV2 ('T':_) = Right ID3.V2p2.parseTextFrame
frameParserV2 "COM"   = Right ID3.V2p2.parseComment
frameParserV2 "PIC"   = Right ID3.V2p2.parsePIC
frameParserV2 "UFI"   = Right ID3.V2p2.parseUFI
frameParserV2 "ULT"   = Right ID3.V2p2.parseULT
frameParserV2 _ = Left "Unhandled frame"

printFrameV2 (ID3.V2p2.TextFrame txt) = TIO.putStrLn txt
printFrameV2 (ID3.V2p2.UniqueFileIdentifierFrame owner _) = putStrLn owner
printFrameV2 (ID3.V2p2.PictureFrame fmt tpe desc _) = print (fmt, tpe, desc)
printFrameV2 (ID3.V2p2.UnsyncLyricsFrame lang desc lyrics) =
    sequence_ [putStr $ show lang ++ " '",
               TIO.putStr desc, putStr "' ",
               TIO.putStrLn lyrics]
printFrameV2 (ID3.V2p2.CommentFrame lang desc com) =
    sequence_ [putStr $ show lang ++ " '",
               TIO.putStr desc, putStr "' '",
               TIO.putStr com,  putStrLn "'"]

handleFramesV2 :: [ID3.V2p2.UnparsedFrame] -> IO ()
handleFramesV2 frames = do
    let (headers, bodies) = unzip frames

        parsers = map (frameParserV2 . fst) headers

    forM_ (zip3 parsers headers bodies) (\(parser, header, body) -> do
        let parse' p = maybe (Left "Failed to parse") (Right) (parse (p header) body)
        putStr $ fst header ++ ": "
        case parser >>= parse' of
            Left err -> putStrLn err
            Right frame -> printFrameV2 frame
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
                Just frames -> putStrLn "" >> handleFramesV2 frames
    where try x = parse x input

someFunc file = do
    fileContents <- openFile file ReadMode >>= L.hGetContents

    putStr $ file ++ ": "

    incrementalParse fileContents
