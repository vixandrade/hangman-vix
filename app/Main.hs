module Main where
import System.Console.ANSI
import Control.Monad.Trans.Maybe
import System.IO

import Game.Auxiliary (getSolution)
import Game.Runner (runGame)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  clearScreen
  putStrLn "CHOOSE A WORDLIST: (batch37, movies, premierleague):"
  wordlist <- getLine 
  solution <- runMaybeT (getSolution wordlist)
  runGame solution
  putStrLn "====================\nGO AGAIN? Y/N"
  again <- getChar
  if again == 'y'
    then do main
    else putStrLn "\nOK, goodbye!"
