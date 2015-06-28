
module AIChallenger.Channel where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

data OutChannel = OutChannel
    { sendLine :: T.Text -> IO ()
    , closeOutChannel :: IO ()
    }

data InChannel = InChannel
    { receiveLine :: IO T.Text
    , channelEOF :: IO Bool
    , closeInChannel :: IO ()
    }

inChannelFromHandle :: Handle -> InChannel
inChannelFromHandle h = InChannel
    (TIO.hGetLine h)
    (hIsEOF h)
    (hClose h)

stdinChannel :: InChannel
stdinChannel = inChannelFromHandle stdin

yesChannel :: T.Text -> InChannel
yesChannel line = InChannel (return line) (return False) (return ())

outChannelFromHandle :: Handle -> OutChannel
outChannelFromHandle h = OutChannel
    (\ln -> TIO.hPutStrLn h ln >> hFlush h)
    (hClose h)

stdoutChannel :: OutChannel
stdoutChannel = outChannelFromHandle stdout

whateverChannel :: OutChannel
whateverChannel = OutChannel (const (return ())) (return ())