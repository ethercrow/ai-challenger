{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Concurrent
import Data.Monoid
import qualified Data.Text as T

import AIChallenger.Bot
import AIChallenger.Channel
import AIChallenger.Match
import AIChallenger.Types
import qualified RockPaperScissors as RPS

constPlayer :: Int -> T.Text -> T.Text -> IO Player
constPlayer i name move = do
    flushVar <- newMVar False
    return Player
        { playerId = PlayerId i
        , playerName = name
        , playerOutput =
            InChannel
                (do
                    flush <- takeMVar flushVar
                    putMVar flushVar (not flush)
                    if flush
                    then return "."
                    else return move)
                (return False)
                (return ())
        , playerInput =
            OutChannel
                (const (return ()))
                (return ())
        , playerClose = return ()
        }


main :: IO ()
main = do
    rocky <- constPlayer 0 "Rocky" "R"
    pepper <- constPlayer 1 "Pepper" "P"
    scarlett <- constPlayer 2 "Scarlett" "S"
    GameResult a b c <- simulateMatch RPS.game (Turn 3) [rocky, pepper]
    putStrLn ("winners: " <> show a)
    putStrLn ("game over type: " <> show b)