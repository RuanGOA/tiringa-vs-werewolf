module Game where

import Maps
import Util
import Position
import Tiringa
import WereWolf
import System.Process
import Data.Matrix
import System.Random

prepare :: Matrix Char -> IO ()
prepare m = do
    let wp = findElementMatrix 'W' m (Data.Matrix.nrows m) :: (Int,Int)
    start m wp

start :: Matrix Char -> (Int, Int) -> IO ()
start m wereWolfPos = do

    -- Select Direction
    system "clear"
    print m
    putStr "Select a direction: "
    dir <- getLine

    -- Tiringa Movement
    let [oldP, newP] = selectDirection m dir 'T'
    let (message1, contin1) = mapper m newP wereWolfPos
    let matrix = moveTiringa m oldP newP
    if (contin1) then do
        -- WereWolf Movement
        let currPos = findElementMatrix 'W' m (Data.Matrix.nrows m) :: (Int,Int)
        let possMov = getPossibleMoves matrix wereWolfPos currPos :: [(Int,Int)]
        n <- randomRIO (0, (Prelude.length possMov) - 1) :: IO Int
        let newPosWereWolf = wereWolfPosition currPos n possMov
        let (message2, contin2) = mapper m newP newPosWereWolf
        let newMatrix = moveWereWolf matrix currPos newPosWereWolf
        if (contin2) then do
            -- Print Matrix
            print newMatrix
            start newMatrix currPos
        else do
            system "clear"
            print newMatrix
            print message2
    else do
        system "clear"
        print matrix
        print message1

wereWolfPosition :: (Int, Int) -> Int -> [(Int,Int)] -> (Int, Int)
wereWolfPosition lastPosition n possMov | (Prelude.length possMov) == 0 = lastPosition
                                   | otherwise = (possMov !! n)

selectDirection :: Matrix Char -> String -> Char -> [(Int, Int)]
selectDirection m dir e | (dir == "w") || (dir == "W") = [p] ++ [moveUp m p]
                        | (dir == "s") || (dir == "S") = [p] ++ [moveDown m p]
                        | (dir == "a") || (dir == "A") = [p] ++ [moveLeft m p]
                        | (dir == "d") || (dir == "D") = [p] ++ [moveRight m p]
                        | otherwise = [p,p]
                        where p = findElementMatrix e m (Data.Matrix.nrows m)

mapper :: Matrix Char -> (Int, Int) -> (Int, Int) -> (String, Bool)
mapper m posTir posWW | (lostTheGame posTir posWW) = ("Game Over!", False)
                      | wonTheGame m posTir = ("You Won! Congratulations!", False)
                      | otherwise = ("", True)
wonTheGame :: Matrix Char -> (Int, Int) -> Bool
wonTheGame m (tx,ty) = (tx == sx) && (ty == sy)
                       where (sx,sy) = findElementMatrix 'S' m (Data.Matrix.nrows m)

lostTheGame :: (Int, Int) -> (Int, Int) -> Bool
lostTheGame (tx, ty) (wx,wy) = (tx == wx) && (ty == wy) 


