module Lib
    ( someFunc
    ) where

import Safe (tailSafe)
import Data.Char (toLower)
import System.Directory
import System.FilePath ((</>), takeExtension)
import Sound.HTagLib

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

isMusicFile :: FilePath -> Bool
isMusicFile = isMusicExtension . map toLower . tailSafe . takeExtension 


someFunc :: FilePath -> IO ()
someFunc rootDir = putStr =<< unlines .  filter isMusicFile <$> find [rootDir]
