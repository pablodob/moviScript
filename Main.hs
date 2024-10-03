module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Control.Concurrent
import GHC.IO.Handle
import System.Process (system)

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
    case parseComm ifile s of
        Right t -> do   putStrLn "------------ AST ------------"
                        print t
                        putStrLn "------------ Estado final ------------"
                        print (eval t)
                        putStrLn "------------ Estado final ------------"
                        printlist (getEnvVar (one (eval t)))
                        putStrLn "------------ Traza ------------"
                        putStrLn (three (eval t))
                        putStrLn "------------ Logo ------------"
                        writeFile "eval.logo" (two (eval t))
                        exitCode <- system "ucblogo eval.logo"
                        print exitCode
        Left error ->   print error

--------------------------------------------------------------------
-- Función auxiliar para imprimir el estado de forma userfriendly --
--------------------------------------------------------------------
printlist [] = return ()
printlist (x:xs) = do putStr (fst x)
                      putStr ": "
                      print (snd x)
                      printlist xs


one :: (a,b,c) -> a
one (x,_,_) = x
two :: (a, b,c) -> b
two (_, x,_) = x
three :: (a, b, c) -> c
three (_, _, x) = x

