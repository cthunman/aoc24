module Day3.Haskell.Solution
  ( runDay3,
  )
where

import Control.Exception (SomeException, try)
import Data.Char (isDigit)

readInputFile :: IO String
readInputFile = do
  result <- try (readFile "inputs/day3/input.txt") :: IO (Either SomeException String)
  case result of
    Left err -> error $ "Error reading file: " ++ show err
    Right content -> return content

dontFindMultiplications :: String -> [(Int, Int)]
dontFindMultiplications [] = []
dontFindMultiplications ('d' : 'o' : '(' : ')' : xs) = doFindMultiplications xs
dontFindMultiplications (_ : xs) = dontFindMultiplications xs

doFindMultiplications :: String -> [(Int, Int)]
doFindMultiplications [] = []
doFindMultiplications ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = dontFindMultiplications xs
doFindMultiplications ('m' : 'u' : 'l' : '(' : xs) =
  case span isDigit (dropWhile (not . isDigit) xs) of
    (x, ',' : rest) ->
      case span isDigit (dropWhile (not . isDigit) rest) of
        (y, ')' : remaining) -> (read x, read y) : doFindMultiplications remaining
        _ -> doFindMultiplications rest
    _ -> doFindMultiplications xs
doFindMultiplications (_ : xs) = doFindMultiplications xs

findMultiplications :: String -> [(Int, Int)]
findMultiplications [] = []
findMultiplications ('m' : 'u' : 'l' : '(' : xs) =
  case span isDigit (dropWhile (not . isDigit) xs) of
    (x, ',' : rest) ->
      case span isDigit (dropWhile (not . isDigit) rest) of
        (y, ')' : remaining) -> (read x, read y) : findMultiplications remaining
        _ -> findMultiplications rest
    _ -> findMultiplications xs
findMultiplications (_ : xs) = findMultiplications xs

sumMultiplications :: [(Int, Int)] -> Int
sumMultiplications = sum . map (\(x, y) -> x * y)

runDay3 :: IO ()
runDay3 = do
  content <- readInputFile
  let p1 = sumMultiplications $ findMultiplications content
  let p2 = sumMultiplications $ doFindMultiplications content
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
