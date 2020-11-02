import Data.Matrix
import Game
import Maps
import System.Exit (exitSuccess)
import System.Process
import System.IO
import Control.Exception
import Data.Time

comoJogar :: IO ()
comoJogar = do
  system "clear"
  putStrLn "====================================="
  putStrLn "=                                   ="
  putStrLn "=           COMO JOGAR?             ="
  putStrLn "=           -----------             ="
  putStrLn "=                                   ="
  putStrLn "= O JOGO CONSISTE EM FAZER  TIRINGA ="
  putStrLn "= ESCAPAR DA CHÁCARA, ANTES  QUE  O ="
  putStrLn "= LOBISOMEM O PEGUE.                ="
  putStrLn "=                                   ="
  putStrLn "= REPRESENTAÇÃO :                   ="
  putStrLn "=   T - TIRINGA                     ="
  putStrLn "=   W - LOBISOMEM                   ="
  putStrLn "=                                   ="
  putStrLn "= COMANDOS :                        ="
  putStrLn "=   W - CIMA                        ="
  putStrLn "=   A - ESQUERDA                    ="
  putStrLn "=   S - BAIXO                       ="
  putStrLn "=   D - DIREITA                     ="
  putStrLn "=                                   ="
  putStrLn "= M - MENU                          ="
  putStrLn "=                                   ="
  putStrLn "====================================="
  opcaoComoJogar <- getLine
  redirecionaComoJogar opcaoComoJogar

redirecionaComoJogar :: String -> IO ()
redirecionaComoJogar st
  | st == "M" || st == "m" = menu
  | otherwise = comoJogar

vencedor :: IO ()
vencedor = do
  system "clear"
  putStrLn "====================================="
  putStrLn "=                                   ="
  putStrLn "=           MELHOR TEMPO            ="
  putStrLn "=             --------              ="
  putStrLn "=                                   ="
  bestPlayer
  putStrLn "=                                   ="
  putStrLn "= M - MENU                          ="
  putStrLn "=                                   ="
  putStrLn "====================================="
  opcaoVencedor <- getLine
  redirecionaVencedor opcaoVencedor

redirecionaVencedor :: String -> IO ()
redirecionaVencedor st
  | st == "M" || st == "m" = menu
  | otherwise = vencedor

dificuldade :: IO ()
dificuldade = do
  system "clear"
  putStrLn "==================================="
  putStrLn "=                                 ="
  putStrLn "=          DIFICULDADE            ="
  putStrLn "=          -----------            ="
  putStrLn "=                                 ="
  putStrLn "= ESCOLHA UMA DIFICULDADE         ="
  putStrLn "=                                 ="
  putStrLn "= 1 - FÁCIL                       ="
  putStrLn "= 2 - TRYHARD                     ="
  putStrLn "=                                 ="
  putStrLn "= M - MENU                        ="
  putStrLn "=                                 ="
  putStrLn "==================================="
  opcaoDificuldade <- getLine
  redirecionaDificuldade opcaoDificuldade

redirecionaDificuldade :: String -> IO ()
redirecionaDificuldade st
  | st == "M" || st == "m" = menu
  | st == "1" = prepare "1"
  | st == "2" = prepare "2"
  | otherwise = dificuldade

menu :: IO ()
menu = do
  system "clear"
  putStrLn "====================================="
  putStrLn "=                                   ="
  putStrLn "=       TIRINGA VS. WEREWOLF        ="
  putStrLn "=       --------------------        ="
  putStrLn "=                                   ="
  putStrLn "= 1 - INICIAR JOGO                  ="
  putStrLn "= 2 - VENCEDOR                      ="
  putStrLn "= 3 - COMO JOGAR?                   ="
  putStrLn "= S - SAIR                          ="
  putStrLn "=                                   ="
  putStrLn "====================================="

  opcaoMenu <- getLine
  redirecionaMenu opcaoMenu

redirecionaMenu :: String -> IO ()
redirecionaMenu st
  | st == "1" = dificuldade 
  | st == "2" = vencedor 
  | st == "3" = comoJogar 
  | st == "S" || st == "s" = exitSuccess
  | otherwise = menu

bestPlayer :: IO()
bestPlayer = do
  arq <- openFile "ranking.txt" ReadMode
  content <- hGetContents arq
  let ranking = words content
  let (bestN, bestM) = getTheBest ((length ranking) - 1) ranking (" ", "99999999999")
  putStrLn ("= " ++ bestN ++ " - " ++ bestM ++ " MOVIMENTOS")
  hClose arq

getTheBest :: Int -> [String] -> (String, String) -> (String, String)

getTheBest (-1) [] _ = ("", "")

getTheBest 1 l (p2,m2) | m1 < m2 = (p1, m1)
                       | otherwise = (p2,m2)
                       where m1 = l Prelude.!! 1
                             p1 = l Prelude.!! 0

getTheBest i l (p, m) | moves < m = getTheBest (i - 2) l (player, moves)
                      | otherwise = getTheBest (i - 2) l (p, m)
                      where player = (l Prelude.!! (i - 1))
                            moves = (l Prelude.!! (i))

main :: IO ()
main = do
  system "clear"
  menu
