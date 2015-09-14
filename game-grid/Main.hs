import AIChallenger

import qualified Grid

main :: IO ()
main = do
    config <- getConfigFromCommandlineFlags
    startJudge Grid.game config