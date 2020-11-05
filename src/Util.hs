module Util (findElementMatrix, findElementMatrixAux) where

import Data.Matrix
import Data.Vector
import Maps

findElementMatrix :: Char -> Matrix Char -> Int -> (Int, Int)
findElementMatrix e m r
  | c == -1 = findElementMatrix e m (r - 1)
  | otherwise = (r, c + 1)
  where
    c = findElementMatrixAux e 0 (getRow r m)

findElementMatrixAux :: Char -> Int -> Vector Char -> Int
findElementMatrixAux e i v
  | i == (Data.Vector.length v) = -1
  | (v Data.Vector.! i) == e = i
  | otherwise = findElementMatrixAux e (i + 1) v