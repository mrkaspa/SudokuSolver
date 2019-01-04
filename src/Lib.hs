module Lib
  ( readGrid
  , showGrid
  , showGridWithPossibilities
  , solve
  ) where

import Control.Applicative ((<|>))
import qualified Data.Char
import Data.Function (on)
import qualified Data.List
import qualified Data.List.Split

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
pruneSubGrids grid = fmap subGridsToRows pruned
  where
    subGrids = subGridsToRows grid
    pruned = traverse pruneRow subGrids

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

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell .
        Data.List.minimumBy (compare `on` (possibilityCount . snd)) .
        filter (isPossible . snd) . zip [0 ..] . concat $
        grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _ = False
    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _) = 1
    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _ = error "Impossible case"
    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9)
      in replace x (replace y (const v))
    replace p f xs =
      [ if i == p
        then f x
        else x
      | (x, i) <- zip xs [0 ..]
      ]

isGridFilled :: Grid -> Bool
isGridFilled grid = null [() | Possible _ <- concat grid]

isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid ||
  any isInvalidRow (Data.List.transpose grid) ||
  any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow row =
      let fixeds = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)
    hasDups l = hasDups' l []
    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise = hasDups' ys (y : xs)

solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g = Just g
      | otherwise =
        let (grid1, grid2) = nextGrids g
        in solve grid1 <|> solve grid2
