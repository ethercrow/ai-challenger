{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module AIChallenger.HTML
    ( MainPage (..)
    , TournamentPage (..)
    , MatchPage (..)
    ) where

import Control.Monad
import Data.FileEmbed
import Data.List (sort, sortOn, intersperse)
import Data.Monoid
import Lucid
import qualified Data.Text as T
import qualified Data.Vector as V

import AIChallenger.Types

newtype MainPage = MainPage ServerState

data TournamentPage = TournamentPage Tournament (V.Vector Match)

data MatchPage = MatchPage Match T.Text

instance ToHtml MainPage where
    toHtml (MainPage (ServerState _ _ _ _ tournaments _)) =
        doctypehtml_ $ do
            h1_ "Welcome!"
            div_ [style_ "float: left"] $ do
                h2_ "Tournaments"
                div_ [id_ "tournaments"] $ do
                    table_ $ do
                        mapM_ (tr_ . td_ . tournamentWidget) tournaments
            div_ [style_ "float: left"] $ do
                h1_ "Place for TV"
            script_ [type_ "text/javascript"] (mainJS tournaments)
    toHtmlRaw = toHtml

mainJS :: V.Vector Tournament -> T.Text
mainJS tournaments = T.unlines
    [ "var tournaments = [" <> T.intercalate ", " (V.toList (V.map showTournament tournaments)) <> "];"
    , $(embedStringFile "src/js/main.js")
    ]
    where
    showTournament (Tournament (TournamentId tid) kind _ matchIds) = mconcat
        [ "["
        , case kind of
            RoundRobin -> "'RoundRobin'"
            Training name -> "'Training " <> name <> "'"
        , ", "
        , showT tid
        , ", "
        , showT (V.length matchIds)
        , "]"
        ]

instance ToHtml TournamentPage where
    toHtml (TournamentPage (Tournament tid@(TournamentId tid') tkind bots matchIds) completedMatches) =
        doctypehtml_ $ do
            h1_ (toHtml ("Tournament #" <> showT tid'))
            div_ (toHtml ("Tournament type: " <> showT tkind))
            div_ [style_ "float: left"] $ do
                h2_ "Scores"
                table_ [id_ "scores"] $ do
                    tr_ $ th_ "Bot" >> th_ "W" >> th_ "D" >> th_ "L"
                    let botsAndScores =
                            sortOn
                                (\(_, w, d, _) -> - 3 * w - d)
                                (V.toList (scoreTournament bots completedMatches))
                    forM_ botsAndScores $ \(bot, w, d, l) -> tr_ $ do
                        td_ (botWidget bot)
                        td_ (toHtml (showT w))
                        td_ (toHtml (showT d))
                        td_ (toHtml (showT l))
            div_ [style_ "float: left"] $ do
                h2_ "Matches"
                table_ $ do
                    tr_ $ do
                        th_ ""
                        foldMap (th_ . botWidget) bots
                    V.forM_ bots $ \b1 -> tr_ $ do
                        td_ (botWidget b1)
                        V.forM_ bots $ \b2 -> do
                            let predicate = (== sort [b1, b2]) . sort . V.toList . matchBots
                                cellId = T.intercalate "_vs_" (map botName [b1, b2])
                            case filter predicate (V.toList completedMatches) of
                                match : _ -> td_ [id_ cellId] (tourneyTableCell match b1)
                                [] -> td_ [id_ cellId] ""
            when (V.length completedMatches < V.length matchIds) $
                script_ [type_ "text/javascript"]
                    (tournamentJS tid (foldMap (pure . botName) bots))
    toHtmlRaw = toHtml

tournamentJS :: TournamentId -> [BotName] -> T.Text
tournamentJS (TournamentId tid) botNames = T.unlines
    [ "var tournamentId = " <> showT tid <> ";"
    , "var botNames = ['" <> T.intercalate "', '" botNames <> "'];"
    , $(embedStringFile "src/js/tournament.js")
    ]

scoreTournament :: V.Vector Bot -> V.Vector Match -> V.Vector (Bot, Int, Int, Int)
scoreTournament bots matches =
    fmap (\bot ->
        let (Sum w, Sum d, Sum l) = foldMap (scoreMatch bot) matches
        in (bot, w, d, l)) bots
    where scoreMatch bot (Match {matchWinners = ws, matchBots = bs})
            | pure bot == ws = (Sum 1, Sum 0, Sum 0)
            | V.length ws == 1 && V.elem bot bs = (Sum 0, Sum 0, Sum 1)
            | V.elem bot bs = (Sum 0, Sum 1, Sum 0)
            | otherwise = (Sum 0, Sum 0, Sum 0)

instance ToHtml MatchPage where
    toHtml (MatchPage (Match (MatchId mid) _ bots winners gameOver _) replayText) =
        doctypehtml_ $ do
            h1_ (toHtml ("Match #" <> showT mid))
            h2_ gameOverMessage
            details_ $ do
                summary_ "Show replay log"
                pre_ (toHtml replayText)
        where
        gameOverMessage =
            case V.toList winners of
                [wbot] -> 
                    let [lbot] = V.toList (V.filter (`V.notElem` winners) bots)
                    in 
                        botWidget wbot
                        <> " defeated " <>
                        botWidget lbot
                        <> " via " <>
                        toHtml (showT gameOver)
                _ ->
                    mconcat (intersperse " and " (map botWidget (V.toList bots)))
                        <> " fought to a draw"
    toHtmlRaw = toHtml

newtype BotWidget = BotWidget Bot

instance ToHtml BotWidget where
    toHtml (BotWidget (Bot name (ExecutableBot exe))) =
        span_ [title_ (showT exe)] (toHtml name)
    toHtml (BotWidget (Bot name _)) = toHtml name
    toHtmlRaw = toHtml

botWidget :: Monad m => Bot -> HtmlT m ()
botWidget = toHtml . BotWidget

newtype TournamentWidget = TournamentWidget Tournament

instance ToHtml TournamentWidget where
    toHtml (TournamentWidget (Tournament (TournamentId tid) tkind _bots tmids)) =
        let text = toHtml
                (showT tkind <> " #" <> showT tid <>
                 " (" <> showT (V.length tmids) <> " matches)")
        in a_ [href_ ("tournaments/" <> showT tid)] text
    toHtmlRaw = toHtml

tournamentWidget :: Monad m => Tournament -> HtmlT m ()
tournamentWidget = toHtml . TournamentWidget

data TourneyTableCell = TourneyTableCell Match Bot

instance ToHtml TourneyTableCell where
    toHtml (TourneyTableCell (Match (MatchId mid) _ _bots winners gameover _) b1) =
        let text = winText <> ":" <> gameoverText
            winText =
                if winners == pure b1 then "W"
                else if V.length winners == 1 then "L"
                else "D"
            gameoverText = case gameover of
                Disqualification _ -> "DQ"
                TurnLimit -> "TL"
                Elimination -> "E"
        in a_ [href_ ("/matches/" <> showT mid)] text
    toHtmlRaw = toHtml

tourneyTableCell :: Monad m => Match -> Bot -> HtmlT m ()
tourneyTableCell match bot = toHtml (TourneyTableCell match bot)