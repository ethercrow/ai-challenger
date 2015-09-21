module AIChallenger.StateVar
    ( StateVar
    , OutChan
    , readChan
    , addBot
    , readStateVar
    , mkTournament
    , mkStateVar
    , addMatch
    , subscribeToStateUpdates
    ) where

-- FIXME: determined user of this module can probably make
--        match id collisions, but we can probably live with that for now

import Control.Concurrent.MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import Control.DeepSeq
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Monoid

import AIChallenger.Types

data StateVar = StateVar (MVar ServerState) (Chan.Chan ServerStateUpdate)

newtype OutChan a = OutChan (Chan.Chan a)

readChan :: OutChan a -> IO a
readChan (OutChan c) = Chan.readChan c

readStateVar :: MonadIO m => StateVar -> m ServerState
readStateVar (StateVar var _) = liftIO (readMVar var)

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
            Chan.writeChan chan u
            return $!! (newValue, Right bot)

addMatch :: MonadIO m => StateVar -> Match -> m ServerState
addMatch stateVar m = modifyStateVar stateVar (AddMatch m)

modifyStateVar :: MonadIO m => StateVar -> ServerStateUpdate -> m ServerState
modifyStateVar (StateVar var chan) u =
    liftIO . modifyMVar var $ \value ->
        let newValue = applyServerStateUpdate u value
        in newValue `deepseq` (do
            Chan.writeChan chan u
            return $! (newValue, newValue))

mkStateVar :: MonadIO m => m StateVar
mkStateVar = StateVar
    <$> liftIO (newMVar (ServerState (MatchId 0) (TournamentId 0) mempty mempty mempty))
    <*> liftIO Chan.newChan

mkTournament :: MonadIO m => StateVar -> TournamentKind -> Int -> m Tournament
mkTournament (StateVar var chan) tournamentKind matchCount = do
    liftIO . modifyMVar var $ \value ->
        let tid = ssNextTournamentId value
            MatchId m = ssNextMatchId value
            mids = V.fromList (fmap MatchId [m .. m + matchCount - 1])
            newTournament = Tournament tid tournamentKind mids
            u = AddTournament newTournament
            newValue = applyServerStateUpdate u value
        in newValue `deepseq` (do
            Chan.writeChan chan u
            return $! (newValue, newTournament)
            )

applyServerStateUpdate :: ServerStateUpdate -> ServerState -> ServerState
applyServerStateUpdate (AddBot bot) ss =
    ss { ssBots = pure bot <> ssBots ss }
applyServerStateUpdate (RemoveBot bot) ss =
    ss { ssBots = V.filter (/= bot) (ssBots ss) }
applyServerStateUpdate (AddMatch match) ss =
    ss { ssMatches = pure match <> ssMatches ss }
applyServerStateUpdate (AddTournament newTournament)
    ss@(ServerState
        { ssNextTournamentId = TournamentId tid
        , ssNextMatchId = MatchId m
        , ssTournaments = tournaments
        }) =
    ss { ssNextTournamentId = TournamentId (tid + 1)
       , ssNextMatchId = MatchId (m + length (tMatchIds newTournament))
       , ssTournaments = pure newTournament <> tournaments
       }

subscribeToStateUpdates :: StateVar -> IO (ServerState, OutChan ServerStateUpdate)
subscribeToStateUpdates (StateVar stateVar chan) =
    withMVar stateVar $ \state -> do
        outChan <- Chan.dupChan chan
        return (state, OutChan outChan)