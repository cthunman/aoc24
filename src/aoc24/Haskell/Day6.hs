module Haskell.Day6
  ( runDay,
  )
where

import Control.Exception (SomeException, try)
import Data.Array
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

type Direction = (Int, Int)

type Position = (Int, Int)

type Arrow = (Position, Direction)

type Grid = Array (Int, Int) Char

readInputFile :: IO String
readInputFile = do
  result <- try (readFile "inputs/day6/input.txt") :: IO (Either SomeException String)
  case result of
    Left err -> error $ "Error reading file: " ++ show err
    Right content -> return content

makeGrid :: String -> Grid
makeGrid input =
  let ls = lines input
      rows = length ls
      cols = length (head ls)
      index_list = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
      chars = [((r, c), (ls !! r !! c)) | (r, c) <- index_list]
   in listArray ((0, 0), (rows - 1, cols - 1)) [snd pair | pair <- chars]

turnRight :: Direction -> Direction
turnRight (dr, dc) = (dc, -dr)

findArrow :: Grid -> Maybe Arrow
findArrow grid =
  let ((minR, minC), (maxR, maxC)) = bounds grid
      positions = [(r, c) | r <- [minR .. maxR], c <- [minC .. maxC]]
      charToDirection '^' = (-1, 0) -- up
      charToDirection '>' = (0, 1) -- right
      charToDirection 'v' = (1, 0) -- down
      charToDirection '<' = (0, -1) -- left
      charToDirection _ = (0, 0) -- not an arrow
   in listToMaybe
        [ (pos, charToDirection char)
          | pos <- positions,
            let char = grid ! pos,
            char `elem` "^>v<"
        ]

addPosition :: Position -> Direction -> Position
addPosition (r, c) (dr, dc) = (r + dr, c + dc)

inBounds :: Position -> Grid -> Bool
inBounds (r, c) grid =
  let ((minR, minC), (maxR, maxC)) = bounds grid
   in r >= minR && r <= maxR && c >= minC && c <= maxC

wouldCreateLoop :: Grid -> Arrow -> Arrow -> Bool
wouldCreateLoop grid (origPos, origDir) (startPos, startDir) =
  let newPos = addPosition startPos startDir
      tempGrid = grid // [(newPos, '#')]
      checkLoop :: Arrow -> Set.Set Arrow -> Bool
      checkLoop (currentPos, currentDir) seen =
        let state = (currentPos, currentDir)
            nextPos = addPosition currentPos currentDir
         in if not (inBounds nextPos tempGrid)
              then False
              else
                if Set.member state seen
                  then True
                  else
                    if tempGrid ! nextPos == '#'
                      then checkLoop (currentPos, turnRight currentDir) (Set.insert state seen)
                      else checkLoop (nextPos, currentDir) seen
   in checkLoop (origPos, origDir) Set.empty

directionToChar :: Direction -> Char
directionToChar (-1, 0) = '^' -- up
directionToChar (0, 1) = '>' -- right
directionToChar (1, 0) = 'v' -- down
directionToChar (0, -1) = '<' -- left
directionToChar _ = '?' -- shouldn't happen

moveArrowWithHistory :: Grid -> Arrow -> Arrow -> Set.Set Arrow -> Int -> Set.Set Position -> (Grid, Int, Set.Set Position)
moveArrowWithHistory grid (origPos, origDir) (pos, dir) seen loopCount loopPositions =
  let newPos = addPosition pos dir
      state = (pos, dir)
      newLoopPositions =
        if inBounds newPos grid
          --   && (not (grid ! newPos == '#'))
          && wouldCreateLoop grid (origPos, origDir) (pos, dir)
          then Set.insert newPos loopPositions
          else loopPositions
      wouldLoop = Set.size newLoopPositions > Set.size loopPositions
      newLoopCount = if wouldLoop then loopCount + 1 else loopCount

      (newGrid, (nextPos, nextDir)) =
        if not (inBounds newPos grid)
          then (grid // [(pos, 'X')], (newPos, dir))
          else
            if grid ! newPos == '#'
              then
                let newDir = turnRight dir
                 in (grid // [(pos, directionToChar newDir), (pos, directionToChar newDir)], (pos, newDir))
              else (grid // [(pos, 'X'), (newPos, directionToChar dir)], (newPos, dir))
   in if not (inBounds nextPos grid) || Set.member state seen
        then (newGrid, newLoopCount, newLoopPositions)
        else
          moveArrowWithHistory
            newGrid
            (origPos, origDir)
            (nextPos, nextDir)
            (Set.insert state seen)
            newLoopCount
            newLoopPositions

moveArrowUntilDone :: Grid -> Arrow -> (Grid, Int, Set.Set Position)
moveArrowUntilDone grid arrow = moveArrowWithHistory grid arrow arrow Set.empty 0 Set.empty

countX :: Grid -> Int
countX grid = length $ filter (== 'X') $ elems grid

runDay :: IO ()
runDay = do
  input <- readInputFile
  let grid = makeGrid input
  case findArrow grid of
    Nothing -> putStrLn "No arrow found in grid!"
    Just arrow -> do
      let (finalGrid, loopCount, _) = moveArrowUntilDone grid arrow
      putStrLn $ "Part 1: " ++ show (countX finalGrid)
      putStrLn $ "Part 2: " ++ show loopCount
