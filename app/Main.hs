module Main where

import qualified Control.Monad
import Lib

main :: IO ()
main = do
  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input -> case readGrid input of
    Nothing   -> putStrLn "Invalid input"
    Just grid -> case solve grid of
      Nothing    -> putStrLn "No solution found"
      Just grid' -> putStrLn $ showGrid grid'
