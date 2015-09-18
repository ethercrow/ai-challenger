{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AIChallenger.Types where

import Control.DeepSeq.Generics
import qualified Data.Aeson as A
import Data.Function (on)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics
import Path
import Path.Internal
import Servant

type BotName = T.Text

data Bot = Bot
    { botName :: !BotName
    , botCommunication :: !BotCommunication
    } deriving (Show, Generic, Eq)

data BotCommunication
    = IdleBot
    | ExecutableBot !(Path Abs File)
    | TCPBot !Int
    deriving (Show, Generic, Eq)

newtype MatchId = MatchId Int
    deriving (Show, Eq, Ord, A.ToJSON, NFData, FromText)

newtype TournamentId = TournamentId Int
    deriving (Eq, Ord, Show, A.ToJSON, NFData, FromText)

data TournamentKind = RoundRobin | Training !BotName
    deriving (Eq, Ord, Show, Generic)

data Tournament = Tournament
    { tId :: !TournamentId
    , tKind :: !TournamentKind
    , tMatchIds :: !(V.Vector MatchId)
    } deriving (Eq, Show, Generic)

data Match = Match
    { matchId :: !MatchId
    , matchTournament :: !TournamentId
    , matchBots :: !(V.Vector Bot)
    , matchWinners :: !(V.Vector Bot)
    , matchGameOverType :: !GameOverType
    , matchReplayPath :: !(Path Abs File)
    } deriving (Eq, Show, Generic)

data ServerState = ServerState
    { ssNextMatchId :: !MatchId
    , ssNextTournamentId :: !TournamentId
    , ssBots :: !(V.Vector Bot)
    , ssMatches :: !(V.Vector Match)
    , ssTournaments :: !(V.Vector Tournament)
    } deriving (Show, Generic)

instance NFData (Path a b) where
    rnf (Path x) = rnf x

instance NFData Bot where
    rnf = genericRnf

instance NFData BotCommunication where
    rnf = genericRnf

instance NFData Match where
    rnf = genericRnf

instance NFData TournamentKind where
    rnf = genericRnf

instance NFData Tournament where
    rnf = genericRnf

instance NFData ServerState where
    rnf = genericRnf

instance A.FromJSON (Path Abs File) where
    parseJSON (A.String t) =
        case parseAbsFile (T.unpack t) of
            Right p -> return p
            Left err -> fail (show err)
    parseJSON _ = mempty

instance A.ToJSON (Path a b) where
    toJSON = A.String . T.pack . toFilePath

instance A.ToJSON a => A.ToJSON (NonEmpty a) where
    toJSON = A.toJSON . toList

instance A.FromJSON Bot

instance A.FromJSON BotCommunication

instance A.ToJSON Bot

instance A.ToJSON BotCommunication

instance A.ToJSON ServerState

instance A.ToJSON Match

instance A.ToJSON Tournament

instance A.ToJSON TournamentKind

data ServerStateUpdate
    = AddBot Bot
    | RemoveBot Bot
    | AddMatch Match
    | AddTournament Tournament
    deriving (Eq, Show, Generic)

newtype Turn = Turn Int
    deriving (Show, Eq, Ord)

newtype Fault = Fault T.Text
    deriving (Show, Eq, NFData, A.ToJSON)

newtype PlayerId = PlayerId Int
    deriving (Show, Eq, Ord, A.ToJSON, NFData)

-- 'Map k v' doesn't have ToJSON/FromJSON, so it's easier to use vector.
-- It's not like we have lots of players in a single match.
type Faults = V.Vector (PlayerId, NonEmpty Fault)

data GameOverType
    = Elimination
    | TurnLimit
    | Disqualification !Faults
    deriving (Show, Generic, Eq)

instance A.ToJSON GameOverType

instance A.ToJSON ServerStateUpdate

instance NFData GameOverType where
    rnf = genericRnf

data GameResult game = GameResult
    { gameWinners :: !(V.Vector PlayerId)
    , gameOverType :: !GameOverType
    , gameReplay :: !(GameReplay game)
    }

instance Show (GameReplay game) => Show (GameResult game) where
    show (GameResult winners reason replay) = unlines
        [ "Game ended in a " <> show reason
        , "Winners: " <> intercalate ", " (V.toList (fmap show winners))
        , "Replay:\n" <> show replay
        ]

class Game game where
    type GameState game
    type GameOrder game
    type GameReplay game
    gameInitialState :: game -> GameState game
    gameParseOrder :: game -> T.Text -> Maybe (GameOrder game)
    gameAdvance :: V.Vector (PlayerId, (V.Vector (GameOrder game))) -> GameState game
        -> Either (GameResult game) (GameState game)
    gameTimeout :: GameState game -> GameResult game
    gameExtractReplay :: game -> GameState game -> GameReplay game
    gameSaveReplay :: game -> Path Abs File -> GameReplay game -> IO ()
    gameSendWorld :: game -> PlayerId -> GameState game -> (T.Text -> IO (Either Fault ())) -> IO ()

instance Monoid Turn where
    mempty = Turn 0
    mappend (Turn x) (Turn y) = Turn (x + y)

instance Enum Turn where
    toEnum = Turn
    fromEnum (Turn x) = x
    succ (Turn x) = Turn (succ x)

instance Ord Bot where
    compare = compare `on` botName

showT :: Show a => a -> T.Text
showT = T.pack . show

data OutChannel = OutChannel
    { sendLine :: T.Text -> IO (Either Fault ())
    , closeOutChannel :: IO ()
    }

data InChannel = InChannel
    { receiveLine :: IO (Either Fault T.Text)
    , channelEOF :: IO Bool
    , closeInChannel :: IO ()
    }