import Data.Matrix
import Data.Vector
import Maps

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