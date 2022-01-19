module Main where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS
import Data.Char (toUpper)
import Data.List
import Data.Text (Text, pack)
import System.Console.ANSI
import System.IO

----

-- Get Solution from user or file (IO)
-- Provided Solution will be constant (Reader)
-- Run GameTurns recursively until guesses match Solution (State)
-- In each turn, if user guesses correct letter, accumulates Score (Writer)

data GameState = GameState
  { guessed :: [Char],
    currentPlayer :: Bool,
    scores :: (Int, Int)
  } deriving (Show)

newGame :: GameState
newGame = GameState {
  guessed = [],
  currentPlayer = True,
  scores = (0,0)
}

type Turn = WriterT Text (ReaderT String (StateT GameState IO))

runTurn :: Turn String
runTurn = do
  solution <- lift ask
  currentState <- lift get
  let guesses = guessed currentState
  let player = currentPlayer currentState
  let masked = getMasked solution guesses
  if masked == solution
    then return "Success"
    else do
      lift $ lift $ lift $ print currentState
      lift $ lift $ lift $ print "=============================="
      lift $ lift $ lift $ print masked
      lift $ lift $ lift $ print (getPlayerName player ++ ": Guess a letter:")
      guess <- lift $ lift $ lift getChar
      let score = getScore guess guesses solution
      tell $ pack (toUpper guess : ": was guessed for " ++ show score ++ " points! \n")
      let newGuesses = guess : guesses
      lift $ modify (updateState score newGuesses)
      lift $ lift $ lift clearScreen
      runTurn

updateState :: Int -> [Char] -> GameState -> GameState
updateState score guesses state =
  GameState
    { guessed = guesses,
      currentPlayer = switchPlayer $ currentPlayer state,
      scores = addScore score (currentPlayer state) (scores state)
    }
  where
    switchPlayer :: Bool -> Bool
    switchPlayer player = not player
    addScore :: Int -> Bool -> (Int, Int) -> (Int, Int)
    addScore score player (p1Score, p2Score)
      | player = (p1Score + score, p2Score)
      | otherwise = (p1Score, p2Score + score)

getPlayerName :: Bool -> String
getPlayerName True = "Player 1"
getPlayerName False = "Player 2"

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
  ((result, log), state) <- runStateT (runReaderT (runWriterT runTurn) "botequim") newGame
  print state

main :: IO ()
main = runGame
