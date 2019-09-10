module Lib
    ( someFunc
    ) where

import System.Directory

someFunc :: FilePath -> IO ()
someFunc rootDir = putStr =<< unlines <$> listDirectory rootDir
