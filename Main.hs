module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Control.Concurrent
import GHC.IO.Handle
import System.IO

import EvalMovi
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg


-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    case (parseComm ifile s) of
        Right t -> do   putStrLn "------------ AST ------------"
                        print t
                        putStrLn "------------ Estado final ------------" 
                        printlist (getEnvVar (fst (eval t)))
                        putStrLn "------------ Traza ------------" 
                        putStrLn (snd (eval t))
        Left error ->   print error

--------------------------------------------------------------------
-- Función auxiliar para imprimir el estado de forma userfriendly --
--------------------------------------------------------------------
printlist [] = return ()
printlist (x:xs) = do putStr (fst x)
                      putStr ": "
                      print (snd x)
                      printlist xs

