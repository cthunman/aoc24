module Main where

import qualified Haskell.Day1 as D1
import qualified Haskell.Day2 as D2
import qualified Haskell.Day3 as D3
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let usage =
        unlines
          [ "Usage:",
            "  cabal run aoc24 -- --day <day-number>",
            "  aoc24 --day <day-number>"
          ]

  case args of
    ["--day", "1"] -> do
      putStrLn "Day 1:"
      D1.runDay
    ["--day", "2"] -> do
      putStrLn "Day 2:"
      D2.runDay
    ["--day", "3"] -> do
      putStrLn "Day 3:"
      D3.runDay
    ["--day", n] ->
      putStrLn $ "Day " ++ n ++ " not implemented yet"
    _ -> putStr usage
