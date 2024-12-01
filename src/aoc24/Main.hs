module Main where

import Day1.Haskell.Solution (runDay1)
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
    ["--day", n] ->
      putStrLn $ "Day " ++ n ++ " not implemented yet"
    _ -> putStr usage
