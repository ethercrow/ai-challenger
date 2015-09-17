{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Chan.Unagi
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid
import Network.Wai
import Network.Wai.Test
import Path
import Test.Tasty.TH
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit
import System.Directory
import System.IO.Unsafe

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

testBot :: String -> Bot
testBot name = Bot (T.pack name) (ExecutableBot (cwd </> fromJust (parseRelFile ("test-grid/test-bots/" <> name <> ".py"))))

assertEqual expected got = liftIO $ HUnit.assertEqual "" expected got

case_get_state :: Assertion
case_get_state = test $ \updates -> do
    resp <- request defaultRequest
    assertStatus 200 resp
    assertContentType "text/html;charset=utf-8" resp

case_tiny_tournament :: Assertion
case_tiny_tournament = test $ \updates -> do
    let bot1 = testBot "bender"
        bot2 = testBot "fry"

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

    resp3 <- reqStartRoundRobinTournament
    liftIO $ print resp3
    assertStatus 201 resp3
    assertContentType "application/json;charset=utf-8" resp3

    up3 <- liftIO $ readChan updates
    assertEqual
        (AddTournament (Tournament
            (TournamentId 0)
            RoundRobin
            (pure (MatchId 0))))
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
