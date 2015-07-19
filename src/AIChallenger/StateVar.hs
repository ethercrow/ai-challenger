module AIChallenger.StateVar
    ( StateVar
    , readStateVar
    , mkStateVar
    , modifyStateVar
    ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.DeepSeq
import qualified Data.Vector as V
import Data.Monoid

import AIChallenger.Types

newtype StateVar = StateVar (MVar ServerState)

readStateVar :: MonadIO m => StateVar -> m ServerState
readStateVar (StateVar var) = liftIO (readMVar var)

modifyStateVar :: MonadIO m => StateVar -> ServerStateUpdate -> m ServerState
modifyStateVar (StateVar var) u =
    liftIO . modifyMVar var $ \value ->
        let newValue = applyServerStateUpdate u value
        in newValue `deepseq` return $! (newValue, newValue)

mkStateVar :: MonadIO m => m StateVar
mkStateVar = StateVar <$> liftIO (newMVar (ServerState mempty mempty))

applyServerStateUpdate (AddBot bot) (ServerState bots matches) =
    ServerState (pure bot <> bots) matches
applyServerStateUpdate (RemoveBot bot) (ServerState bots matches) =
    ServerState (V.filter (/= bot) bots) matches