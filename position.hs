module Position where

import Data.Matrix
import Data.Vector

moveUp :: Matrix Char -> (Int, Int) -> (Int, Int)
moveUp m (r,c) | r <= 1 = (r,c)
               | otherwise = if checkWallPosition m (r - 1, c) then (r,c)
                             else (r - 1, c)

moveDown :: Matrix Char -> (Int, Int) -> (Int, Int)
moveDown m (r,c) | r == Data.Matrix.nrows m = (r,c)
                 | otherwise = if checkWallPosition m (r + 1, c) then (r, c)
                               else (r + 1, c)

moveRight :: Matrix Char -> (Int, Int) -> (Int, Int)
moveRight m (r,c) | c == Data.Matrix.ncols m = (r,c)
                  | otherwise = if checkWallPosition m (r, c + 1) then (r, c)
                                else (r, c + 1)

moveLeft :: Matrix Char -> (Int, Int) -> (Int, Int)
moveLeft m (r,c) | c <= 1 = (r,c)
                 | otherwise = if checkWallPosition m (r, c - 1) then (r, c)
                               else (r, c - 1)

checkWallPosition :: Matrix Char -> (Int, Int) -> Bool
checkWallPosition m (r, c) | getElem r c m == '#' = True
                           | otherwise = False