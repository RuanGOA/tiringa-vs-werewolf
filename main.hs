import System.Process
import System.Exit (exitSuccess)
import Game
import Maps

comoJogar :: IO()
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
	system "clear"
	redirecionaComoJogar opcaoComoJogar


redirecionaComoJogar:: String -> IO()
redirecionaComoJogar st
	| st == "M" = menu
	| otherwise = comoJogar


vencedor :: IO()
vencedor = do
	putStrLn "====================================="
	putStrLn "=                                   ="
	putStrLn "=             VENCEDOR              ="
	putStrLn "=             --------              ="
	putStrLn "=                                   ="
	putStrLn "=    -->  ALGUEM 13:42:36  <--      ="
	putStrLn "=                                   ="
	putStrLn "= M - MENU                          ="
	putStrLn "=                                   ="
	putStrLn "====================================="
	opcaoVencedor <- getLine
	system "clear"
	redirecionaVencedor opcaoVencedor


redirecionaVencedor :: String -> IO()
redirecionaVencedor st
	| st == "M" = menu
	| otherwise = vencedor


dificuldade :: IO()
dificuldade = do
	putStrLn "==================================="
	putStrLn "=                                 ="
	putStrLn "=          DIFICULDADE            ="
	putStrLn "=          -----------            ="
	putStrLn "=                                 ="
	putStrLn "= ESCOLHA UMA DIFICULDADE         ="
	putStrLn "=                                 ="
	putStrLn "= 1 - FÁCIL                       ="
	putStrLn "= 2 - MÉDIO                       ="
	putStrLn "=                                 ="
	putStrLn "= M - MENU                        ="
	putStrLn "=                                 ="
	putStrLn "==================================="
	opcaoDificuldade <- getLine
	system "clear"
	redirecionaDificuldade opcaoDificuldade


redirecionaDificuldade :: String -> IO()
redirecionaDificuldade st
	| st == "M" = menu
	| st == "1" = dificuldade
	| st == "2" = dificuldade 
	| otherwise = dificuldade


menu :: IO()
menu = do
	putStrLn "====================================="
	putStrLn "=                                   ="
	putStrLn "=       TIRINGA VS. WEREWOLF        ="
	putStrLn "=       --------------------        ="
	putStrLn "=                                   ="
	putStrLn "= 1 - INICIAR JOGO                  ="
	putStrLn "= 2 - VENCEDOR                      ="
	putStrLn "= 3 - SELECIONAR DIFICULDADE        ="
	putStrLn "= 4 - COMO JOGAR?                   ="
	putStrLn "= 5 - SAIR                          ="
	putStrLn "=                                   ="
	putStrLn "====================================="	

	opcaoMenu <- getLine
	system "clear"
	redirecionaMenu opcaoMenu 


redirecionaMenu :: String -> IO()
redirecionaMenu st
	| st == "1" = prepare m
	| st == "2" = vencedor
	| st == "3" = dificuldade 
	| st == "4" = comoJogar
	| st == "5" = exitSuccess
	| otherwise = menu


main :: IO()
main = do
	system "clear"
	menu
	
