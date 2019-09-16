module Lib
    ( someFunc
    ) where

import Safe (tailSafe)
import Data.Char (toLower)
import Data.Text (Text)
import System.Directory
import System.FilePath ((</>), takeExtension)

-- (a -> IO Bool) -> (a -> bool) -> (a -> b) -> a -> IO b
musicExtensions = ["mp3", "ogg", "flac", "wav"]

isMusicExtension = flip elem musicExtensions

find :: [FilePath] -> IO [FilePath]
find [] = return []
find (path:paths) = do
    dir <- doesDirectoryExist path
    file <- doesFileExist path
    let rest = find paths
    if dir then do
        list <- map (path </>) <$> listDirectory path
        let rest' = (++) <$> find list <*> rest
        (:) <$> pure path <*> rest'
    else if file then
        (:) <$> pure path <*> rest
    else rest

normExtension = map toLower . tailSafe . takeExtension

isMusicFile :: FilePath -> Bool
isMusicFile = isMusicExtension . normExtension

someFunc :: FilePath -> IO ()
someFunc rootDir = do
    musicFiles <- filter isMusicFile <$> find [rootDir]
    putStr $ unlines  musicFiles
