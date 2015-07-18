{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AIChallenger.Types where

import Control.DeepSeq.Generics
import qualified Data.Aeson as A
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics
import Path
import Path.Internal

data Bot = Bot
    { botName :: !T.Text
    , botExecutable :: !(Path Abs File)
    } deriving (Show, Generic)

data ServerState = ServerState
    { ssBots :: V.Vector Bot
    } deriving Generic

instance NFData (Path a b) where
    rnf (Path x) = rnf x

instance NFData Bot where
    rnf = genericRnf

instance NFData ServerState where
    rnf = genericRnf

instance A.FromJSON (Path Abs File) where
    parseJSON (A.String t) =
        case parseAbsFile (T.unpack t) of
            Right p -> return p
            Left err -> fail (show err)

instance A.ToJSON (Path a b) where
    toJSON = A.String . T.pack . toFilePath

instance A.FromJSON Bot

instance A.ToJSON Bot

instance A.ToJSON ServerState

data ServerStateUpdate
    = AddExecutable (Path Abs File)
    | RemoveExecutable (Path Abs File)

newtype Turn = Turn Int
    deriving (Show, Eq, Ord)

newtype Fault = Fault T.Text
    deriving Show

newtype PlayerId = PlayerId Int
    deriving (Show, Eq, Ord)

type Faults = M.Map PlayerId (NonEmpty Fault)

data GameOverType
    = Elimination
    | TurnLimit
    | Disqualification Faults
    deriving Show

data GameResult game = GameResult
    { gameWinners :: [PlayerId]
    , gameOverType :: GameOverType
    , gameReplay :: GameReplay game
    }

instance Show (GameResult game) where
    show (GameResult winners reason replay) = unlines
        [ "Game ended in a " <> show reason
        , "Winners: " <> intercalate ", " (map show winners)
        ]

class Game game where
    type GameState game
    type GameOrder game
    type GameReplay game
    gameInitialState :: game -> GameState game
    gameParseOrder :: game -> T.Text -> Maybe (GameOrder game)
    gameAdvance :: M.Map PlayerId [GameOrder game] -> GameState game
        -> Either (GameResult game) (GameState game)
    gameTimeout :: GameState game -> GameResult game
    gameExtractReplay :: game -> GameState game -> GameReplay game

instance Monoid Turn where
    mempty = Turn 0
    mappend (Turn x) (Turn y) = Turn (x + y)

instance Enum Turn where
    succ (Turn x) = (Turn (succ x))