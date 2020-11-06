module Util (findElementMatrix, findElementMatrixAux, printMatrix) where

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

printMatrix :: Matrix Char -> Int -> IO()
printMatrix m r = do
    if (r /= (Data.Matrix.nrows m) + 1) 
        then do
            let row = printMatrixAux (getRow r m) 0
            let strRow = row
            putStrLn (strRow)
            printMatrix m (r + 1)
        else do
            putStrLn ""

printMatrixAux :: Vector Char -> Int -> String
printMatrixAux v i
  | i == (Data.Vector.length v) = ""
  | otherwise = (function (v Data.Vector.! i)) Prelude.++ (printMatrixAux v (i + 1))

function :: Char -> String
function c = (" " Prelude.++ [c] Prelude.++ " ")