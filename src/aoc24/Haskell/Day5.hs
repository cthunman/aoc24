module Haskell.Day5
  ( runDay,
  )
where

import Control.Exception (SomeException, try)
import qualified Data.Graph as G
import Data.List (findIndex, nub, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Pair = (Int, Int)

type NumberList = [Int]

getMiddle :: NumberList -> Maybe Int
getMiddle [] = Nothing
getMiddle xs = Just $ xs !! (length xs `div` 2)

readInputFile :: IO String
readInputFile = do
  result <- try (readFile "inputs/day5/input.txt") :: IO (Either SomeException String)
  case result of
    Left err -> error $ "Error reading file: " ++ show err
    Right content -> return content

parseInput :: String -> ([Pair], [NumberList])
parseInput input = case splitOn "\n\n" input of
  [pairsSection, listsSection] ->
    let pairs = map parsePair $ lines pairsSection
        numberLists = map parseNumberList $ lines listsSection
     in (pairs, numberLists)
  _ -> error "Invalid input format"

parsePair :: String -> Pair
parsePair line = case splitOn "|" line of
  [x, y] -> (read x, read y)
  _ -> error "Invalid pair format"

parseNumberList :: String -> NumberList
parseNumberList line = map read $ splitOn "," line

followsRules :: [Pair] -> NumberList -> Bool
followsRules pairs numbers = all (followsRule numbers) pairs

followsRule :: NumberList -> Pair -> Bool
followsRule numbers (a, b) = case (findIndex (== a) numbers, findIndex (== b) numbers) of
  (Just indexA, Just indexB) -> indexA < indexB
  _ -> True -- if either number isn't in the list, that's fine

getPassingLists :: [Pair] -> [NumberList] -> [NumberList]
getPassingLists pairs numberLists = filter (followsRules pairs) numberLists

getFailingLists :: [Pair] -> [NumberList] -> [NumberList]
getFailingLists pairs numberLists = filter (not . followsRules pairs) numberLists

reorderList :: [Pair] -> NumberList -> NumberList
reorderList pairs numbers =
  let uniqueNums = nub numbers
      edges = [(a, b) | (a, b) <- pairs, a `elem` uniqueNums, b `elem` uniqueNums]
      (graph, vertexToNode, _) = G.graphFromEdges 
          [(n, n, [b | (a, b) <- edges, a == n]) | n <- uniqueNums]
      sortedVertices = G.topSort graph
      sortedNums = map (\v -> let (n, _, _) = vertexToNode v in n) sortedVertices
      posMap = Map.fromList $ zip sortedNums ([0..] :: [Int])
      compareByPos a b =
        compare
          (Map.findWithDefault (maxBound :: Int) a posMap)
          (Map.findWithDefault (maxBound :: Int) b posMap)
   in sortBy compareByPos numbers

part1 :: [Pair] -> [NumberList] -> Int
part1 pairs numberLists = sum $ map (maybe 0 id . getMiddle) $ getPassingLists pairs numberLists

part2 :: [Pair] -> [NumberList] -> Int
part2 pairs numberLists = sum $ map (maybe 0 id . getMiddle) $ map (reorderList pairs) $ getFailingLists pairs numberLists

runDay :: IO ()
runDay = do
  input <- readInputFile
  if null input
    then putStrLn "No input data!"
    else do
      let (pairs, numberLists) = parseInput input
      putStrLn $ "Part 1: " ++ show (part1 pairs numberLists)
      putStrLn $ "Part 2: " ++ show (part2 pairs numberLists)
