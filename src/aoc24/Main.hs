module Main where

import Day1.Haskell.Solution (runDay1)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--day", "1"] -> do
            putStrLn "Day 1:"
            runDay1
        ["--day", n] -> 
            putStrLn $ "Day " ++ n ++ " not implemented yet"
        _ -> putStrLn "Usage: cabal run aoc24 --day <day-number>"
