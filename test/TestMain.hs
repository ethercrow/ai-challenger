{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Vector as V
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
    putStrLn "All done"

constPlayersTest :: IO ()
constPlayersTest = do
    rocky <- constPlayer 0 "Rocky" "R"
    pepper <- constPlayer 1 "Pepper" "P"
    GameResult winners gameover replay <- simulateMatch RPS.game (Turn 3) [rocky, pepper]
    True <- return $! winners == V.fromList [PlayerId 2]
    True <- return $! TurnLimit == gameover
    True <- return $! replay == RPS.Replay (V.map (\i -> (RPS.WinCounts 0 i, RPS.R, RPS.P)) [1, 2, 3])
    return ()

pythonPlayersTest :: IO ()
pythonPlayersTest = do
    curDir <- parseAbsDir =<< getCurrentDirectory
    let rocky = Bot "Rocky" (ExecutableBot (curDir </> $(mkRelFile "game-rps/rock.py")))
        pepper = Bot "Pepper" (ExecutableBot (curDir </> $(mkRelFile "game-rps/paper.py")))
        bots = [rocky, pepper]
    Match (MatchId 0) bots' winners gameover <- launchBotsAndSimulateMatch RPS.game (Turn 3) bots (MatchId 0)
    True <- return $! bots == bots'
    True <- return $! winners == [pepper]
    True <- return $! TurnLimit == gameover
    return ()