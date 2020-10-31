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
  system "cls"
  redirecionaComoJogar opcaoComoJogar

redirecionaComoJogar :: String -> Matrix Char -> IO ()
redirecionaComoJogar st m
  | st == "M" || st == "m" = menu m
  | otherwise = comoJogar m

vencedor :: IO ()
vencedor = do
  putStrLn "====================================="
  putStrLn "=                                   ="
  putStrLn "=           MELHOR TEMPO            ="
  putStrLn "=             --------              ="
  putStrLn "=                                   ="
  putStrLn "=                                   ="
  putStrLn "=                                   ="
  putStrLn "= M - MENU                          ="
  putStrLn "=                                   ="
  putStrLn "====================================="
  opcaoVencedor <- getLine
  system "cls"
  redirecionaVencedor opcaoVencedor

redirecionaVencedor :: String -> Matrix Char -> IO ()
redirecionaVencedor st m
  | st == "M" || st == "m" = menu m
  | otherwise = vencedor m

dificuldade :: IO ()
dificuldade = do
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
  system "cls"
  redirecionaDificuldade opcaoDificuldade

redirecionaDificuldade :: String -> Matrix Char -> IO ()
redirecionaDificuldade st m
  | st == "M" || st == "m" = menu m
  | otherwise = prepare m st

menu :: IO ()
menu = do
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
  system "cls"
  redirecionaMenu opcaoMenu


bestPlayer :: IO()
bestPlayer = do
  arq <- openFile "ranking.txt" ReadMode
  content <- hGetContents arq
  let ranking = words content
  let (bestN, bestM) = getTheBest ((length ranking) - 1) ranking (" ", "99999999999")
  putStrLn ("The Best Player: " ++ bestN ++ " " ++ bestM)
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

redirecionaMenu :: String -> Matrix Char -> IO ()
redirecionaMenu st m
  | st == "1" = dificuldade m
  | st == "2" = vencedor m
  | st == "3" = comoJogar m
  | st == "S" || st == "s" = exitSuccess
  | otherwise = menu m


main :: IO ()
main = do
  system "cls"
  menu
