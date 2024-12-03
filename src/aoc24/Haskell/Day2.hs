module Haskell.Day2
  ( runDay,
  )
where

import Control.Exception (SomeException, try)

readInputFile :: IO [[Int]]
readInputFile = do
  result <- try (readFile "inputs/day2/input.txt") :: IO (Either SomeException String)
  case result of
    Left err -> error $ "Error reading file: " ++ show err
    Right content -> return $ map parseLine $ lines content

parseLine :: String -> [Int]
parseLine = map read . words

isDecreasingSequence :: [Int] -> Bool
isDecreasingSequence [] = True
isDecreasingSequence [_] = True
isDecreasingSequence (x : y : xs) =
  let diff = x - y
   in diff >= 1 && diff <= 3 && isDecreasingSequence (y : xs)

isDecreasingDampenedSequenceFirst :: [Int] -> Bool
isDecreasingDampenedSequenceFirst [] = True
isDecreasingDampenedSequenceFirst [_] = True
isDecreasingDampenedSequenceFirst (x : y : xs) =
  let diff = x - y
   in (diff >= 1 && diff <= 3 && isDecreasingDampenedSequence (y : xs))
        || isDecreasingSequence (x : xs)
        || isDecreasingSequence (y : xs)

isDecreasingDampenedSequence :: [Int] -> Bool
isDecreasingDampenedSequence [] = True
isDecreasingDampenedSequence [_] = True
isDecreasingDampenedSequence (x : y : xs) =
  let diff = x - y
   in (diff >= 1 && diff <= 3 && isDecreasingDampenedSequence (y : xs))
        || isDecreasingSequence (x : xs)

isIncreasingSequence :: [Int] -> Bool
isIncreasingSequence [] = True
isIncreasingSequence [_] = True
isIncreasingSequence (x : y : xs) =
  let diff = y - x
   in diff >= 1 && diff <= 3 && isIncreasingSequence (y : xs)

isIncreasingDampenedSequenceFirst :: [Int] -> Bool
isIncreasingDampenedSequenceFirst [] = True
isIncreasingDampenedSequenceFirst [_] = True
isIncreasingDampenedSequenceFirst (x : y : xs) =
  let diff = y - x
   in (diff >= 1 && diff <= 3 && isIncreasingDampenedSequence (y : xs))
        || isIncreasingSequence (x : xs)
        || isIncreasingSequence (y : xs)

isIncreasingDampenedSequence :: [Int] -> Bool
isIncreasingDampenedSequence [] = True
isIncreasingDampenedSequence [_] = True
isIncreasingDampenedSequence (x : y : xs) =
  let diff = y - x
   in (diff >= 1 && diff <= 3 && isIncreasingDampenedSequence (y : xs))
        || isIncreasingSequence (x : xs)

part1 :: [[Int]] -> Int
part1 = length . filter (\nums -> isIncreasingSequence nums || isDecreasingSequence nums)

part2 :: [[Int]] -> Int
part2 =
  length
    . filter
      ( \nums ->
          isIncreasingDampenedSequenceFirst nums
            || isDecreasingDampenedSequenceFirst nums
      )

runDay :: IO ()
runDay = do
  input <- readInputFile
  if null input
    then putStrLn "No input data!"
    else do
      let p1 = part1 input
      putStrLn $ "Part 1: " ++ show p1
      let p2 = part2 input
      putStrLn $ "Part 2: " ++ show p2
