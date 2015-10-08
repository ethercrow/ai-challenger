{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import AIChallenger.HTML
import AIChallenger.Types
import qualified Data.Vector as V
import Lucid
import Path

main :: IO ()
main = do
    let rocky = Bot "rocky" (ExecutableBot $(mkAbsFile "/opt/rocky.py"))
        pepper = Bot "pepper" (ExecutableBot $(mkAbsFile "/opt/pepper.py"))
        scarlett = Bot "scarlett" (ExecutableBot $(mkAbsFile "/opt/scarlett.py"))

        rvp = Match
            (MatchId 42)
            tid
            (V.fromList [rocky, pepper])
            (V.fromList [pepper])
            Elimination
            $(mkAbsFile "/tmp/42.replay")

        rvs = Match
            (MatchId 43)
            tid
            (V.fromList [rocky, scarlett])
            (V.fromList [rocky])
            Elimination
            $(mkAbsFile "/tmp/43.replay")

        pvs = Match
            (MatchId 44)
            tid
            (V.fromList [pepper, scarlett])
            (V.fromList [scarlett])
            Elimination
            $(mkAbsFile "/tmp/44.replay")

        tid = TournamentId 3

        tournament = Tournament
            (TournamentId 3)
            RoundRobin
            (V.fromList [rocky, pepper, scarlett])
            (V.fromList [MatchId 42, MatchId 43, MatchId 44])

        state = ServerState
            (MatchId 45)
            (TournamentId 11)
            (V.fromList [rocky, pepper, scarlett])
            (V.fromList [rvp, rvs, pvs])
            (pure tournament)

    save "lorem-main.html" (MainPage state)

    save "lorem-tournament.html"
        (TournamentPage
            tournament
            (V.fromList [rvp, rvs, pvs]))

    save "lorem-match.html"
        (MatchPage rvp "R P\n.\nR P\n.\n")

save :: ToHtml a => FilePath -> a -> IO ()
save dst stuff = renderToFile dst (toHtml stuff)