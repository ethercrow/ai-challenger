import AIChallenger

import qualified RockPaperScissors as RPS

main :: IO ()
main = do
    config <- getConfigFromCommandlineFlags
    startJudge RPS.game config