# hangman-vix
Haskell implementation of Hangman – my way!

The Hangman game is a classic exercise to study Functional Programming, as its game loop can be easily implemented with a recursive function, with clear conditions for ending the game.

I've changed the rules of the game to make it complex enough to introduce Monads and Transformers in the exercise.

This version has two players competing for the highest score – each correct guess adds to the score depending on the guessed letter.

The word to be used as solution is loaded from a dictionary file. A of the guesses and awarded points is shown at the end of the game.
