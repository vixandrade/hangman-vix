module Game.Auxiliary where

import System.Directory
import Control.Monad.Random
import Control.Monad.Trans.Maybe

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