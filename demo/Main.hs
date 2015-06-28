{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import qualified RockPaperScissors as RPS
import AIChallenger

main :: IO ()
main = startJudge RPS.game (Turn 100) =<< getArgs