{-# LANGUAGE OverloadedStrings #-}

module AIChallenger.HTML
    ( MatchPage (..)
    ) where

import Control.Monad (forM_)
import Data.Monoid
import Lucid

import AIChallenger.Types

newtype MatchPage = MatchPage Match

instance ToHtml MatchPage where
    toHtml (MatchPage (Match mid bots winners gameover)) =
        doctypehtml_ $ do
            p_ (toHtml ("Match #" <> show mid))
            p_ "Bots:"
            ul_ $ foldMap (li_ . toHtml . botName) bots
            p_ "Winners:"
            ul_ $ foldMap (li_ . toHtml . botName) winners
            p_ "Game over type: "
            case gameover of
                Elimination -> "elimination"
                TurnLimit -> "turn limit"
                Disqualification faults -> do
                    p_ "Disqualification"
                    table_ $ do
                        tr_ (foldMap th_ ["Bot", "Fault"])
                        forM_ faults $ \(pid, fs) -> do
                            th_ (toHtml (show pid))
                            foldMap (th_ . toHtml . show) fs