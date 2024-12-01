module Day1.Haskell.Solution
  ( runDay1
  )
where

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import System.IO (readFile)
import Control.Exception (try, SomeException)

readInputFile :: IO [(Int, Int)]
readInputFile = do
  result <- try (readFile "inputs/day1/input.txt") :: IO (Either SomeException String)
  case result of
    Left e -> do
      putStrLn $ "Error reading file: " ++ show e
      return []
    Right contents -> do
      return $ map parseLine $ lines contents

parseLine :: String -> (Int, Int)
parseLine line =
  let tokens = words line
  in case tokens of
    [n1, n2] -> (read n1, read n2)
    _ -> error $ "Invalid line format: " ++ line

part1 :: [(Int, Int)] -> Int
part1 pairs =
  sum $
    zipWith
      (\n1 n2 -> abs (n1 - n2))
      (sort $ map fst pairs)
      (sort $ map snd pairs)

frequencies :: Ord a => [a] -> Map a Int
frequencies = foldr (\a -> Map.insertWith (+) a 1) Map.empty

part2 :: [(Int, Int)] -> Int
part2 pairs =
  let list1 = map fst pairs
      list2 = map snd pairs
      freq1 = frequencies list2
   in sum $ map (\n1 -> n1 * Map.findWithDefault 0 n1 freq1) list1

runDay1 :: IO ()
runDay1 = do
  putStrLn "Starting Day 1..."
  input <- readInputFile
  if null input 
    then putStrLn "No input data!"
    else do
      let p1 = part1 input
      let p2 = part2 input
      putStrLn $ "Part 1: " ++ show p1
      putStrLn $ "Part 2: " ++ show p2
