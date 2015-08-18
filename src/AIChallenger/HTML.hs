{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AIChallenger.HTML
    ( MatchPage (..)
    , MainPage (..)
    ) where

import Control.Monad (forM_)
import Data.Monoid
import Lucid
import qualified Data.Text.Lazy as TL

import AIChallenger.Types

data MatchPage = MatchPage
    { mpResult :: Match
    , mpReplayText :: TL.Text
    }

newtype MainPage = MainPage ServerState

instance ToHtml MatchPage where
    toHtml (MatchPage (Match (MatchId mid) bots winners gameover _) replayText) =
        doctypehtml_ $ do
            p_ (toHtml ("Match #" <> show mid))
            p_ "Bots:"
            ul_ $ foldMap (li_ . toHtml . botName) bots
            p_ "Winners:"
            ul_ $ foldMap (li_ . toHtml . botName) winners
            span_ "Game over type: "
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
            p_ "Replay:"
            pre_ (toHtml replayText)

instance ToHtml MainPage where
    toHtml (MainPage (ServerState _ bots matches)) =
        doctypehtml_ $ do
            h1_ "Welcome!"
            h2_ "Bots"
            mapM_ (p_ . toHtml . botName) bots
            h2_ "Matches"
            mapM_ (p_ . toHtml . show) matches