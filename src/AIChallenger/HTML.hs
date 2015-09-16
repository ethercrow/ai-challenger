{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AIChallenger.HTML
    ( MainPage (..)
    ) where

import Data.List (sort)
import Data.Monoid
import Lucid
import qualified Data.Text as T
import qualified Data.Vector as V

import AIChallenger.Types

newtype MainPage = MainPage ServerState

instance ToHtml MainPage where
    toHtml (MainPage (ServerState _ _ bots matches _tournaments)) =
        doctypehtml_ $ do
            h1_ "Welcome!"
            h2_ "Bots"
            mapM_ (p_ . botWidget) bots
            h2_ "Matches"
            table_ $ do
                tr_ $ do
                    th_ ""
                    foldMap (th_ . botWidget) bots
                V.forM_ bots $ \b1 -> tr_ $ do
                    td_ (botWidget b1)
                    V.forM_ bots $ \b2 -> do
                        let predicate = (== (sort [b1, b2])) . sort . V.toList . matchBots
                        case filter predicate (V.toList matches) of
                            match : _ -> td_ (matchWidget match b1)
                            [] -> td_ ""
    toHtmlRaw = toHtml

newtype BotWidget = BotWidget Bot

instance ToHtml BotWidget where
    toHtml (BotWidget (Bot name (ExecutableBot exe))) =
        span_ [title_ (T.pack (show exe))] (toHtml name)
    toHtml (BotWidget (Bot name _)) = toHtml name
    toHtmlRaw = toHtml

botWidget :: Monad m => Bot -> HtmlT m ()
botWidget = toHtml . BotWidget

data MatchWidget = MatchWidget Match Bot

instance ToHtml MatchWidget where
    toHtml (MatchWidget (Match (MatchId mid) _ _ winners gameover _) b1) =
        let text = winText <> ":" <> gameoverText
            winText =
                if winners == pure b1 then "W"
                else if V.length winners == 1 then "L"
                else "D"
            gameoverText = case gameover of
                Disqualification _ -> "DQ"
                TurnLimit -> "TL"
                Elimination -> "E"
        in a_ [href_ ("match/" <> T.pack (show mid))] text
    toHtmlRaw = toHtml

matchWidget :: Monad m => Match -> Bot -> HtmlT m ()
matchWidget match bot = toHtml (MatchWidget match bot)