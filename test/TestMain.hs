{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent
import Data.Monoid
import qualified Data.Text as T
import Path
import System.Directory

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
                    then return (Right ".")
                    else return (Right move))
                (return False)
                (return ())
        , playerInput =
            OutChannel
                (const (return (Right ())))
                (return ())
        , playerAdditionalShutdown = return ()
        }


main :: IO ()
main = do
    constPlayersTest
    pythonPlayersTest

constPlayersTest :: IO ()
constPlayersTest = do
    rocky <- constPlayer 0 "Rocky" "R"
    pepper <- constPlayer 1 "Pepper" "P"
    GameResult winners gameover replay <- simulateMatch RPS.game (Turn 3) [rocky, pepper]
    putStrLn ("winners: " <> show winners)
    putStrLn ("game over type: " <> show gameover)
    putStrLn ("replay: " <> show replay)

pythonPlayersTest :: IO ()
pythonPlayersTest = do
    curDir <- parseAbsDir =<< getCurrentDirectory
    let bots =
            [ Bot "Rocky" (ExecutableBot (curDir </> $(mkRelFile "game-rps/rock.py")))
            , Bot "Pepper" (ExecutableBot (curDir </> $(mkRelFile "game-rps/paper.py")))
            ]
    Match (MatchId 0) bots' winners gameover <- launchBotsAndSimulateMatch RPS.game (Turn 3) bots (MatchId 0)
    True <- return $! bots == bots'
    putStrLn ("winners: " <> show winners)
    putStrLn ("game over type: " <> show gameover)