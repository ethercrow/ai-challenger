module AIChallenger.StateVar
    ( StateVar
    , addBot
    , readStateVar
    , mkTournament
    , mkStateVar
    , addMatch
    , subscribeToStateUpdates
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi
import Control.Monad.IO.Class
import Control.DeepSeq
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Monoid

import AIChallenger.Types

data StateVar = StateVar (MVar ServerState) (InChan ServerStateUpdate)

readStateVar :: MonadIO m => StateVar -> m ServerState
readStateVar (StateVar var _) = liftIO (readMVar var)

takeNextMatchId :: MonadIO m => StateVar -> m MatchId
takeNextMatchId (StateVar var _) =
    liftIO . modifyMVar var $ \value ->
        let mid@(MatchId x) = ssNextMatchId value
            newValue = value { ssNextMatchId = MatchId (x + 1) }
        in newValue `deepseq` return $! (newValue, mid)

takeNextTournamentId :: MonadIO m => StateVar -> m TournamentId
takeNextTournamentId (StateVar var _) =
    liftIO . modifyMVar var $ \value ->
        let tid@(TournamentId x) = ssNextTournamentId value
            newValue = value { ssNextTournamentId = TournamentId (x + 1) }
        in newValue `deepseq` return $! (newValue, tid)

addBot :: MonadIO m => Bot -> StateVar -> m (Either String Bot)
addBot bot (StateVar var chan) = do
    let name = botName bot
    liftIO . modifyMVar var $ \value ->
        if any ((== name) . botName) (ssBots value)
        then 
            return $!! (value, Left ("Bot named '" <> T.unpack name <> "' already exists"))
        else do
            let u = AddBot bot
                newValue = applyServerStateUpdate u value
            writeChan chan u
            return $!! (newValue, Right bot)

addMatch :: MonadIO m => StateVar -> Match -> m ServerState
addMatch stateVar m = modifyStateVar stateVar (AddMatch m)

modifyStateVar :: MonadIO m => StateVar -> ServerStateUpdate -> m ServerState
modifyStateVar (StateVar var chan) u =
    liftIO . modifyMVar var $ \value ->
        let newValue = applyServerStateUpdate u value
        in newValue `deepseq` (do
            writeChan chan u
            return $! (newValue, newValue))

mkStateVar :: MonadIO m => m StateVar
mkStateVar = StateVar
    <$> liftIO (newMVar (ServerState (MatchId 0) (TournamentId 0) mempty mempty mempty))
    <*> (fst <$> liftIO newChan)

mkTournament :: MonadIO m => StateVar -> TournamentKind -> Int -> m Tournament
mkTournament (StateVar var chan) tournamentKind matchCount = do
    liftIO . modifyMVar var $ \value ->
        let tid@(TournamentId t) = ssNextTournamentId value
            MatchId m = ssNextMatchId value
            mids = V.fromList (fmap MatchId [m .. m + matchCount - 1])
            newTournament = Tournament tid tournamentKind mids
            u = AddTournament newTournament
            newValue = value
                { ssNextTournamentId = TournamentId (t + 1)
                , ssNextMatchId = MatchId (m + matchCount)
                , ssTournaments = pure newTournament <> ssTournaments value
                }
        in newValue `deepseq` (do
            writeChan chan u
            return $! (newValue, newTournament)
            )

applyServerStateUpdate :: ServerStateUpdate -> ServerState -> ServerState
applyServerStateUpdate (AddBot bot) ss =
    ss { ssBots = pure bot <> ssBots ss }
applyServerStateUpdate (RemoveBot bot) ss =
    ss { ssBots = V.filter (/= bot) (ssBots ss) }
applyServerStateUpdate (AddMatch match) ss =
    ss { ssMatches = pure match <> ssMatches ss }

subscribeToStateUpdates :: StateVar -> IO (ServerState, OutChan ServerStateUpdate)
subscribeToStateUpdates (StateVar stateVar chan) =
    withMVar stateVar $ \state -> do
        outChan <- dupChan chan
        return (state, outChan)