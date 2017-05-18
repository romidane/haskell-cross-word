module Lib
  ( formatGrid
  , outputGrid
  , findWord
  , findWordInLine
  , findWords
  , skew
  ) where

import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes, mapMaybe)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

getLines :: Grid -> [String]
getLines grid =
  let horizantal = grid
      vertical = transpose horizantal
      diagonal = transpose $ skew horizantal
      reverseDiagonal = transpose $ skew . reverse $ horizantal
      lines' = horizantal ++ vertical ++ diagonal ++ reverseDiagonal
  in lines' ++ map reverse lines'

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let girdLines = getLines grid
  in foldr
       (\line acc ->
          if findWordInLine word line
            then Just word
            else acc)
       Nothing
       girdLines

findWords :: Grid -> [String] -> [String]
findWords grid = mapMaybe (findWord grid)

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where
    indent :: String -> String
    indent line = '_' : line
