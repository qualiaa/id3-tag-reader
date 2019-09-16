module Main where

import System.Environment (getArgs)
import ParseMP3

main :: IO ()
main = head <$> getArgs >>= someFunc
