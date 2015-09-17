{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Chan.Unagi
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid
import GHC.Stats
import Network.Wai
import Network.Wai.Test
import Path
import Test.Tasty.TH
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit
import System.Directory
import System.IO.Unsafe
import System.Mem

import AIChallenger.StateVar (mkStateVar, subscribeToStateUpdates)
import AIChallenger.Types
import AIChallenger.WebApp (webApp)
import qualified Grid

main :: IO ()
main = $defaultMainGenerator

test :: (OutChan ServerStateUpdate -> Session b) -> IO b
test stuff = do
    stateVar <- mkStateVar
    (_, updates) <- subscribeToStateUpdates stateVar
    runSession (stuff updates) (webApp Grid.game stateVar Nothing $(mkAbsDir "/dummy/dashboard/dir"))

reqStartRoundRobinTournament :: Session SResponse
reqStartRoundRobinTournament = srequest
    (SRequest
        (defaultRequest
            { pathInfo = ["launch-round-robin-tournament"]
            , requestMethod = "POST"
            , requestHeaders =
                [ ("Accept", "application/json")
                ]
            })
        mempty)

reqAddBot :: Bot -> Session SResponse
reqAddBot bot = srequest
    (SRequest
        (defaultRequest
            { pathInfo = ["add-bot"]
            , requestMethod = "POST"
            , requestHeaders =
                [ ("Accept", "application/json")
                , ("Content-Type", "application/json")
                ]
            })
        (A.encode bot))

cwd :: Path Abs Dir
cwd = unsafePerformIO (parseAbsDir =<< getCurrentDirectory)

testBot :: String -> String -> Bot
testBot name fn = Bot (T.pack name) (ExecutableBot (cwd </> fromJust (parseRelFile ("test-grid/test-bots/" <> fn))))

assertEqual :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
assertEqual expected got = liftIO $ HUnit.assertEqual "" expected got

case_get_state :: Assertion
case_get_state = test $ \_updates -> do
    resp <- request defaultRequest
    assertStatus 200 resp
    assertContentType "text/html;charset=utf-8" resp

case_tiny_tournament :: Assertion
case_tiny_tournament = test $ \updates -> do
    let bot1 = testBot "bender" "bender.py"
        bot2 = testBot "fry" "fry.py"

    resp1 <- reqAddBot bot1
    liftIO $ print resp1
    assertStatus 201 resp1
    assertContentType "application/json;charset=utf-8" resp1

    up1 <- liftIO $ readChan updates
    assertEqual (AddBot bot1) up1

    resp2 <- reqAddBot bot2
    liftIO $ print resp2
    assertStatus 201 resp2
    assertContentType "application/json;charset=utf-8" resp2

    up2 <- liftIO $ readChan updates
    assertEqual (AddBot bot2) up2

    let tournament = Tournament (TournamentId 0) RoundRobin (pure (MatchId 0))

    resp3 <- reqStartRoundRobinTournament
    liftIO $ print resp3
    assertStatus 201 resp3
    assertContentType "application/json;charset=utf-8" resp3
    assertBody (A.encode tournament) resp3

    up3 <- liftIO $ readChan updates
    assertEqual
        (AddTournament tournament)
        up3

    up4 <- liftIO $ readChan updates
    assertEqual
        (AddMatch (Match
            (MatchId 0)
            (TournamentId 0)
            (pure bot1 <> pure bot2)
            (pure bot2)
            Elimination
            $(mkAbsFile "/tmp/0.replay")))
        up4

case_40_player_tournament :: Assertion
case_40_player_tournament = test $ \updates -> do

    gcStatsBefore <- liftIO (performGC >> getGCStats)

    let botCount = 40
        matchCount = botCount * (botCount - 1) `div` 2
        bots = [testBot ("bot" <> show i) "fry.py" | i <- [1 .. botCount]]

    resps <- mapM reqAddBot bots
    mapM_ (assertStatus 201) resps

    liftIO $ replicateM_ botCount (readChan updates)

    resp3 <- reqStartRoundRobinTournament
    liftIO $ print resp3
    assertStatus 201 resp3
    assertContentType "application/json;charset=utf-8" resp3

    let tournament = Tournament
            (TournamentId 0)
            RoundRobin
            (V.fromList (fmap MatchId [0 .. matchCount - 1]))
    assertBody (A.encode tournament) resp3

    up3 <- liftIO $ readChan updates
    assertEqual
        (AddTournament tournament)
        up3
    liftIO $ replicateM_ matchCount (readChan updates)
    
    gcStatsAfter <- liftIO (performGC >> getGCStats)

    let diff field = field gcStatsAfter - field gcStatsBefore
        cpuSpent = diff cpuSeconds
        wallSpent = diff wallSeconds
        cpuGCSpent = diff gcCpuSeconds
        allocs = diff bytesAllocated
        maxResidency = maxBytesUsed gcStatsAfter

    liftIO $ do
        putStrLn "------------------------"
        putStrLn "Large tournament stats:"
        putStrLn ("  Wall seconds spent: " <> show wallSpent)
        putStrLn ("  CPU seconds spent: " <> show cpuSpent)
        putStrLn ("  CPU seconds spent on GC: " <> show cpuGCSpent)
        putStrLn ("  MiB allocated: " <> show (allocs `div` (1024 * 1024)))
        putStrLn ("  Maximum residency, MiB: " <> show (maxResidency `div` (1024 * 1024)))
        putStrLn "------------------------"

        assertBool "Too much CPU spent" (cpuSpent < 30)
        assertBool "Unacceptable max memory residency" (maxResidency < 10 * 1024 * 1024)