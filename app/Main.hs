module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS
import Data.Char (toUpper)
import Data.List
import Data.Text (Text, pack, unpack)
import System.Console.ANSI
import System.IO
import System.Directory

----

data GameState = GameState
  { guessed :: [Char],
    currentPlayer :: Bool,
    scores :: (Int, Int)
  }
  deriving (Show)

newGame :: GameState
newGame =
  GameState
    { guessed = [],
      currentPlayer = True,
      scores = (0, 0)
    }

type Turn = WriterT Text (ReaderT String (StateT GameState IO))

runTurn :: Turn String
runTurn = do
  lift $ lift $ lift clearScreen
  solution <- lift ask
  currentState <- lift get
  let guesses = guessed currentState
  let player = currentPlayer currentState
  let masked = getMasked solution guesses
  if masked == solution
    then return "Success"
    else do
      lift $ lift $ lift $ putStrLn "=============================="
      lift $ lift $ lift $ putStrLn (">> " ++ masked ++ " <<")
      lift $ lift $ lift $ putStrLn (getPlayerName player ++ ": Guess a letter:")
      guess <- lift $ lift $ lift getChar
      let score = getScore guess guesses solution
      tell $ pack (toUpper guess : ": was guessed by " ++ getPlayerName player ++ " for " ++ show score ++ " points! \n")
      let newGuesses = guess : guesses
      lift $ modify (updateState score newGuesses)
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

getSolution :: String -> MaybeT IO String
getSolution wordlist = do
  let wordlistFile = "wordlists/" ++ wordlist ++ ".txt"
  fileExists <- lift $ doesFileExist wordlistFile
  if not fileExists
    then do
      lift $ print "Wordlist not available!"
      MaybeT (return Nothing)
    else do
      fileContents <- lift $ readFile wordlistFile
      let words = lines fileContents
      rand <- getRandomR (0, length words - 1)
      return (words !! rand)

runGame :: Maybe String -> IO ()
runGame Nothing = main
runGame (Just solution) = do
  hSetBuffering stdin NoBuffering
  ((result, log), state) <- runStateT (runReaderT (runWriterT runTurn) solution) newGame
  let p1Score = show $ fst (scores state)
  let p2Score = show $ snd (scores state)
  putStrLn "\nROUND ENDED!"
  putStrLn $ "PLAYER 1: " ++ p1Score ++ " points"
  putStrLn $ "PLAYER 2: " ++ p2Score ++ " points"
  putStrLn "HERE'S HOW IT WENT:"
  putStrLn $ unpack log
  putStrLn "====================\nPLAY AGAIN? Y/N"
  again <- getChar
  if again == 'y'
    then do main
    else putStrLn "\nOK, goodbye!"
  

main :: IO ()
main = do
  clearScreen
  putStrLn "CHOOSE A WORDLIST: (batch37, movies, premierleague):"
  wordlist <- getLine 
  solution <- runMaybeT (getSolution wordlist)
  runGame solution
