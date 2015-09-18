
module AIChallenger.Config
    ( Config(..)
    , defaultConfig
    , getConfigFromCommandlineFlags
    ) where

import Control.Applicative
import Data.Maybe
import Options.Applicative
import Options.Applicative.Builder.Internal (HasValue)
import Path
import System.Directory

import AIChallenger.Types

data Config = Config
    { cfgPort :: Int
    , cfgAddress :: String
    , cfgTurnLimit :: Turn
    , cfgBotExecutables :: [Path Abs File]
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgPort = 8081
    , cfgAddress = "127.0.0.1"
    , cfgTurnLimit = Turn 200
    , cfgBotExecutables = []
    }

getConfigFromCommandlineFlags :: IO Config
getConfigFromCommandlineFlags = do
    cwd <- parseAbsDir =<< getCurrentDirectory
    let parseFileName fn = parseAbsFile fn <|> fmap (cwd </>) (parseRelFile fn)
    execParser (info (parseConfig parseFileName) (progDesc "ai-challenger"))

parseConfig :: (String -> Maybe (Path Abs File)) -> Parser Config
parseConfig parseFileName = Config
    <$> option auto
        (long "port" <> def "HTTP port" cfgPort)
    <*> strOption
        (long "address" <> def "HTTP address" cfgAddress)
    <*> fmap Turn (option auto
            (long "turn-limit" <> def "turn limit" (fromTurn . cfgTurnLimit)))
    <*> fmap (mapMaybe parseFileName) (many (argument str (metavar "BOT_EXECUTABLE")))

def :: (Show a, Options.Applicative.Builder.Internal.HasValue t) => String -> (Config -> a) -> Mod t a
def msg field =
    let defaultValue = field defaultConfig
    in help (msg <> ", default is " <> show defaultValue) <> value defaultValue