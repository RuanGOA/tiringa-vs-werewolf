module WereWolf where
import Data.Matrix
import Position
import Util
import Data.Vector
import Maps
import Data.List

ordMovements :: Matrix Char -> (Int, Int) -> (Int, Int) -> [(Int, Int, Float)]
ordMovements m tir wereWolf = sortBy compares (getDistance tir moves)
                            where moves = getPossibleMoves m wereWolf

compares :: (Ord a, Ord b, Ord c) => (a, b, c) -> (a, b, c) -> Ordering
compares (a1, b1, c1) (a2, b2, c2) | c1 > c2 = GT -- Greater Than
                                   | c1 == c2 = EQ -- EQUAL
                                   | otherwise = LT -- LESS THAN

moveWereWolf :: Matrix Char -> (Int, Int) -> (Int,Int) -> Matrix Char
moveWereWolf m old new = setElem 'W' new (setElem ' ' old m)

getPossibleMoves :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getPossibleMoves m (cx,cy) = [(x,y) | (x,y) <- (allPositions m (cx,cy)), 
                                      ((not $ checkIsExit m (x,y)) && 
                                      (not $ checkisEqual (x,y) (cx,cy)))]

allPositions :: Matrix Char -> (Int, Int) -> [(Int, Int)]
allPositions m (x, y) = [moveUp m (x,y), moveDown m (x,y),
                         moveRight m (x,y), moveLeft m (x,y)]

checkisEqual :: (Int, Int) -> (Int, Int) -> Bool
checkisEqual (a,b) (c,d) = (a == c && b == d)

checkIsExit :: Matrix Char -> (Int, Int) -> Bool
checkIsExit m (r, c) | getElem r c m == 'S' = True
                     | otherwise = False

getDistance :: (Int, Int) -> [(Int, Int)] ->  [(Int, Int, Float)]
getDistance _ [] = []
getDistance tir (x:xs) = [euclideanDistance tir x] Prelude.++ (getDistance tir xs)

euclideanDistance :: (Int, Int) -> (Int, Int) -> (Int, Int, Float)
euclideanDistance (tx, ty) (wx, wy) = (wx, wy, sqrt (((wxf - txf) ** 2) + ((wyf - tyf) ** 2)))
                                      where wxf = fromInteger $ toInteger wx :: Float
                                            wyf = fromInteger $ toInteger wy :: Float
                                            txf = fromInteger $ toInteger tx :: Float
                                            tyf = fromInteger $ toInteger ty :: Float
