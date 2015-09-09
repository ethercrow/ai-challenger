module AIChallenger.StateVar
    ( StateVar
    , readStateVar
    , takeNextMatchId
    , mkStateVar
    , modifyStateVar
    , subscribeToStateUpdates
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi
import Control.Monad.IO.Class
import Control.DeepSeq
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

modifyStateVar :: MonadIO m => StateVar -> ServerStateUpdate -> m ServerState
modifyStateVar (StateVar var chan) u =
    liftIO . modifyMVar var $ \value ->
        let newValue = applyServerStateUpdate u value
        in newValue `deepseq` (do
            writeChan chan u
            return $! (newValue, newValue))

mkStateVar :: MonadIO m => m StateVar
mkStateVar = StateVar
    <$> liftIO (newMVar (ServerState (MatchId 0) mempty mempty))
    <*> (fst <$> liftIO newChan)

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