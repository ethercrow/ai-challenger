{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Maybe
import Path
import System.Environment

import AIChallenger

import qualified Grid

main :: IO ()
main = do
    self <- parseAbsFile =<< getExecutablePath
    let selfDir = parent self
        parseFileName fn =
            parseAbsFile fn <|> fmap (selfDir </>) (parseRelFile fn)
    exes <- fmap (catMaybes . fmap parseFileName) getArgs
    startJudge Grid.game exes