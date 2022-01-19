module Main where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS
import Data.Char (toUpper)
import Data.List
import Data.Text (Text, pack)
import System.IO
import System.Console.ANSI

----

-- Get Solution from user or file (IO)
-- Provided Solution will be constant (Reader)
-- Run GameTurns recursively until guesses match Solution (State)
-- In each turn, if user guesses correct letter, accumulates Score (Writer)

type Turn = WriterT Text (ReaderT String (StateT [Char] IO))

runTurn :: Turn String
runTurn = do
  solution <- lift ask
  guesses <- lift get
  let masked = getMasked solution guesses
  if masked == solution
    then return "Success"
    else do
      lift $ lift $ lift $ print "=============================="
      lift $ lift $ lift $ print masked
      lift $ lift $ lift $ print "Guess a letter:"
      guess <- lift $ lift $ lift getChar
      let score = getScore guess guesses solution
      tell $ pack (toUpper guess : ": was guessed for " ++ show score ++ " points! \n")
      let newGuesses = guess : guesses
      lift $ put newGuesses
      lift $ lift $ lift clearScreen
      runTurn

getMasked :: String -> [Char] -> String
getMasked solution guesses = [if c `elem` guesses then c else '_' | c <- solution]

getScore :: Char -> [Char] -> String -> Int
getScore c guesses solution
  | c `elem` guesses = 0
  | c `notElem` solution = 0
  | c `elem` ['a', 'e', 'i', 'o', 'u'] = 5
  | otherwise = 10

runGame :: IO ()
runGame = do
  hSetBuffering stdin NoBuffering
  ((result, log), state) <- runStateT (runReaderT (runWriterT runTurn) "botequim") []
  print log

main :: IO ()
main = runGame
