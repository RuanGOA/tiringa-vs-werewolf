module Game where

import Data.Matrix
import Position
import System.Process
import Tiringa
import Util
import WereWolf
import Maps
import System.IO
import System.Random
import Control.Exception
import Data.Time

maps = [m1, m2, m3, m4, m5]

prepare :: String -> IO ()
prepare dif = do
  putStr "Type your name: "
  name <- getLine
  indexMap <- randomRIO (0, (Prelude.length maps) - 1) :: IO Int
  let m = (maps Prelude.!! indexMap) :: (Matrix Char)

  let wp = findElementMatrix 'W' m (Data.Matrix.nrows m) :: (Int, Int)
  let tp = findElementMatrix 'T' m (Data.Matrix.nrows m) :: (Int, Int)
  start m wp tp dif name 0

start :: Matrix Char -> (Int, Int) -> (Int, Int) -> String -> String -> Int -> IO ()
start m wereWolfPos tiringaPosition dif name movements = do
  startTime <- getCurrentTime
  -- Select Direction
  system "cls"
  print m
  putStr "Select a direction: "
  dir <- getLine

  -- Tiringa Movement
  let newPosTiringa = selectDirection m dir tiringaPosition
  let (message1, contin1) = mapper m newPosTiringa wereWolfPos
  let matrix = moveTiringa m tiringaPosition newPosTiringa
  if (contin1)
    then do
      -- WereWolf Movement
      let possMov = ordMovements m newPosTiringa wereWolfPos
      let (wx, wy, dw) = possMov Prelude.!! 0
      let newPosWereWolf = (wx, wy)
      let (message2, contin2) = mapper matrix newPosTiringa newPosWereWolf
      let newMatrix = moveWereWolf matrix wereWolfPos newPosWereWolf
      if (contin2)
        then do
          if (dif == "2") 
            then do
              let possMov2 = ordMovements newMatrix newPosTiringa newPosWereWolf
              let (wx2, wy2, dw2) = possMov2 Prelude.!! 0
              let newPosWereWolf2 = (wx2, wy2)
              let (message22, contin22) = mapper newMatrix newPosTiringa newPosWereWolf2
              let newMatrix2 = moveWereWolf newMatrix newPosWereWolf newPosWereWolf2
              if (contin22) 
                then do
                  start newMatrix2 newPosWereWolf2 newPosTiringa dif name (movements + 1)
                else do
                  -- Lost the game
                  system "cls"
                  print newMatrix2
                  print message22
          else do
            start newMatrix newPosWereWolf newPosTiringa dif name (movements + 1)
        else do
          -- Lost the game
          system "cls"
          print newMatrix
          print message2
    else do
      -- Win the game
      endTime <- getCurrentTime
      arq <- openFile "ranking.txt" AppendMode
      hPutStr arq (name ++ " " ++ (show movements) ++ "\n")
      hFlush arq
      hClose arq
      system "cls"
      print matrix
      print message1

wereWolfPosition :: (Int, Int) -> Int -> [(Int, Int)] -> (Int, Int)
wereWolfPosition lastPosition n possMov
  | (Prelude.length possMov) == 0 = lastPosition
  | otherwise = (possMov !! n)

selectDirection :: Matrix Char -> String -> (Int, Int) -> (Int, Int)
selectDirection m dir oldP
  | (dir == "w") || (dir == "W") = moveUp m oldP
  | (dir == "s") || (dir == "S") = moveDown m oldP
  | (dir == "a") || (dir == "A") = moveLeft m oldP
  | (dir == "d") || (dir == "D") = moveRight m oldP
  | otherwise = oldP

mapper :: Matrix Char -> (Int, Int) -> (Int, Int) -> (String, Bool)
mapper m posTir posWW
  | (lostTheGame posTir posWW) = ("Game Over!", False)
  | wonTheGame m posTir = ("You Won! Congratulations!", False)
  | otherwise = ("", True)

wonTheGame :: Matrix Char -> (Int, Int) -> Bool
wonTheGame m (tx, ty) = (tx == sx) && (ty == sy)
  where
    (sx, sy) = findElementMatrix 'S' m (Data.Matrix.nrows m)

lostTheGame :: (Int, Int) -> (Int, Int) -> Bool
lostTheGame (tx, ty) (wx, wy) = (tx == wx) && (ty == wy)
