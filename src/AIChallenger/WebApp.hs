{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.WebApp
    ( webApp
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Wai as Wai
import Servant
import Servant.HTML.Lucid

import AIChallenger.HTML
import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

type WebAPI
    = Get '[HTML] MainPage
    :<|> "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot
    :<|> "launch-tournament" :> Post '[PlainText] T.Text
    :<|> "replay" :> Capture "matchId" MatchId :> Get '[HTML] MatchPage

webApp :: Game game => game -> StateVar -> Wai.Application
webApp game stateVar =
    let handlers = mainPage stateVar
            :<|> readStateVar stateVar
            :<|> postBot stateVar
            :<|> launchTournament game stateVar
            :<|> replay stateVar
    in serve (Proxy :: Proxy WebAPI) handlers

postBot :: MonadIO m => StateVar -> Bot -> m Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    _ <- modifyStateVar stateVar (AddBot bot)
    return bot

launchTournament :: (Game game, MonadIO m) => game -> StateVar -> m T.Text
launchTournament game stateVar = do
    ServerState _ bots _ <- readStateVar stateVar
    let pairs = [V.fromList [b1, b2] | b1 <- bots, b2 <- bots, b1 < b2]
    _ <- liftIO . forkIO $ do
        V.forM_ pairs $ \pair -> do
            mid <- takeNextMatchId stateVar
            result <- launchBotsAndSimulateMatch game (Turn 100) pair mid
            _ <- modifyStateVar stateVar (AddMatch result)
            return ()
    return (T.pack (show (length pairs) <> " matches scheduled"))

replay :: MonadIO m => StateVar -> MatchId -> m MatchPage
replay stateVar mid = do
    ServerState _ _ matches <- readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [match] -> return (MatchPage match)
        _ -> error "match id collision, this should never happen"

mainPage :: MonadIO m => StateVar -> m MainPage
mainPage stateVar = MainPage <$> readStateVar stateVar