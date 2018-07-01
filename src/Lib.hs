module Lib
  ( someFunc
  , readGrid
  , showGrid
  , showGridWithPossibilities
  , pruneGrid
  ) where

import qualified Data.Char
import qualified Data.List
import qualified Data.List.Split

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Cell
  = Fixed Int
  | Possible [Int]
  deriving (Show, Eq)

type Row = [Cell]

type Grid = [Row]

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 =
    traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise = Nothing
  where
    readCell '.' = Just $ Possible [1 .. 9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just $ Fixed $ Data.Char.digitToInt c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x ++ "          "
    showCell (Possible xs) =
      (++ "]") .
      Data.List.foldl'
        (\acc x ->
           acc ++
           if x `elem` xs
             then show x
             else " ")
        "[" $
      [1 .. 9]

pruneRow :: Row -> Maybe Row
pruneRow cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]
    pruneCell (Possible xs) =
      case xs Data.List.\\ fixeds of
        [] -> Nothing
        [y] -> Just $ Fixed y
        ys -> Just $ Possible ys
    pruneCell x = Just x

pruneRows :: Grid -> Maybe Grid
pruneRows grid = traverse pruneRow grid

pruneColumns :: Grid -> Maybe Grid
pruneColumns grid = fmap Data.List.transpose traversed
  where
    revGrid = Data.List.transpose grid
    traversed = traverse pruneRow revGrid

pruneSubGrids :: Grid -> Maybe Grid
pruneSubGrids grid = pruneRows subGrids
  where
    subGrids = subGridsToRows grid

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid = do
  grid1 <- pruneRows grid
  grid2 <- pruneColumns grid1
  grid3 <- pruneSubGrids grid2
  return grid3

pruneGrid'' :: Grid -> Maybe Grid
pruneGrid'' grid =
  traverse pruneRow grid >>=
  fmap Data.List.transpose . traverse pruneRow . Data.List.transpose >>=
  fmap subGridsToRows . traverse pruneRow . subGridsToRows

recPruning :: (Grid -> Maybe Grid) -> Grid -> Maybe Grid
recPruning f x =
  f x >>= \x' ->
    if x' == x
      then return x
      else recPruning f x'

pruneGrid :: Grid -> Maybe Grid
pruneGrid = recPruning pruneGrid'

subGridsToRows :: Grid -> Grid
subGridsToRows =
  concatMap
    (\rows ->
       let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) rows
       in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3) .
  Data.List.Split.chunksOf 3
