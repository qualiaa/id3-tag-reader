module Main where

import System.Environment (getArgs)
import ID3

main :: IO ()
main = mapM_ someFunc =<< getArgs
