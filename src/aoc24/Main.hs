module Main where

import Day1.Haskell.Solution (runDay1)
import Day2.Haskell.Solution (runDay2)
import Day3.Haskell.Solution (runDay3)
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
      runDay1
    ["--day", "2"] -> do
      putStrLn "Day 2:"
      runDay2
    ["--day", "3"] -> do
      putStrLn "Day 3:"
      runDay3
    ["--day", n] ->
      putStrLn $ "Day " ++ n ++ " not implemented yet"
    _ -> putStr usage
