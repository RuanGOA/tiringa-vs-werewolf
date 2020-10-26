module WereWolf where
import Data.Matrix
import Position
import Util
import Data.Vector
import Maps

moveWereWolf :: Matrix Char -> (Int, Int) -> (Int,Int) -> Matrix Char
moveWereWolf m old new = setElem 'W' new (setElem ' ' old m)

getPossibleMoves :: Matrix Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getPossibleMoves m (ox,oy) (cx,cy) =  [(x,y) | (x,y) <- (allPositions m (cx,cy)), 
                                    ((not $ checkIsExit m (x,y)) 
                                    && (not $ checkisEqual (x,y) (ox,oy))
                                    && (not $ checkisEqual (x,y) (cx,cy)))]

allPositions :: Matrix Char -> (Int, Int) -> [(Int, Int)]
allPositions m (x, y) = [moveUp m (x,y), moveDown m (x,y),
                         moveRight m (x,y), moveLeft m (x,y)]

checkisEqual :: (Int, Int) -> (Int, Int) -> Bool
checkisEqual (a,b) (c,d) = (a == c && b == d)

checkIsExit :: Matrix Char -> (Int, Int) -> Bool
checkIsExit m (r, c) | getElem r c m == 'S' = True
                     | otherwise = False