module Tiringa where

import Data.Matrix
import Data.Vector

isExit :: Matrix Char -> (Int, Int) -> Bool
isExit m (r,c) | Data.Matrix.getElem r c m == 'S' = True
               | otherwise = False

moveTiringa :: Matrix Char -> (Int,Int) -> (Int,Int) -> Matrix Char
moveTiringa m old new = setElem 'T' new (setElem ' ' old m)