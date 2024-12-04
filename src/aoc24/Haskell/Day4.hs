module Haskell.Day4
  ( runDay,
  )
where

import Control.Exception (SomeException, try)
import Data.Array

type Grid = Array (Int, Int) Char

readInputFile :: IO String
readInputFile = do
  result <- try (readFile "inputs/day4/input.txt") :: IO (Either SomeException String)
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

directions :: [(Int, Int)]
directions =
  [ (0, 1),
    (1, 0),
    (0, -1),
    (-1, 0),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1)
  ]

inBounds :: (Int, Int) -> Grid -> Bool
inBounds (r, c) grid =
  let ((minR, minC), (maxR, maxC)) = bounds grid
   in r >= minR && r <= maxR && c >= minC && c <= maxC

getWord :: Grid -> (Int, Int) -> (Int, Int) -> Int -> String
getWord grid (r, c) (dr, dc) len =
  [grid ! (r + i * dr, c + i * dc) | i <- [0 .. len - 1], inBounds (r + i * dr, c + i * dc) grid]

findXMAS :: Grid -> [(Int, Int, String)] -- returns positions and direction found
findXMAS grid =
  [ (r, c, "Found in direction: " ++ show (dr, dc))
    | (r, c) <- indices grid,
      (dr, dc) <- directions,
      let word = getWord grid (r, c) (dr, dc) 4,
      word == "XMAS"
  ]

getWordX :: Grid -> (Int, Int) -> (Int, Int) -> String
getWordX grid (r, c) (dr, dc) =
  let center = [grid ! (r, c) | inBounds (r, c) grid]
      forward = [grid ! (r + dr, c + dc) | inBounds (r + dr, c + dc) grid]
      backward = [grid ! (r - dr, c - dc) | inBounds (r - dr, c - dc) grid]
   in backward ++ center ++ forward

findMASMAS :: Grid -> [(Int, Int, String)] -- returns positions
findMASMAS grid =
  [ (r, c, "Found MASMAS at A: (" ++ show r ++ "," ++ show c ++ ")")
    | (r, c) <- indices grid,
      grid ! (r, c) == 'A', -- find center A's
      let word1 = getWordX grid (r, c) (1, 1), -- check MAS in one diagonal
      let word2 = getWordX grid (r, c) (1, -1), -- check MAS in opposite diagonal
      (word1 == "MAS" || word1 == "SAM") && (word2 == "MAS" || word2 == "SAM")
  ]

part1 :: String -> Int
part1 input =
  let grid = makeGrid input
      matches = findXMAS grid
   in length matches

part2 :: String -> Int
part2 input =
  let grid = makeGrid input
      matches = findMASMAS grid
   in length matches

runDay :: IO ()
runDay = do
  input <- readInputFile
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
