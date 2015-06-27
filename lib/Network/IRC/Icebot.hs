{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- icebot - bot for #snowdrift on FreeNode
-- Copyright (c) 2015, Peter Harpending.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public
-- License along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

-- |
-- Module      : Network.IRC.Icebot
-- Description : Umbrella module for the icebot library
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Network.IRC.Icebot
  ( -- ** Convenience re-exports
    module Control.Exceptional
    -- * Configuration
  , IceConfig(..)
  , Server(..)
  , readConfig
  , readConfigFile
    -- * Running the server
  , runServer
  , commandsParser
  , Message(..)
  , Command (..)
    -- *** Semantic aliases for 'String'
  , Channel
  , Person
  )
  where

import Control.Applicative (Alternative(..))
import Control.Concurrent
import Control.Exceptional
import Control.Monad
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml hiding (Parser)
import Network
import System.Directory
import System.IO

-- |The Icebot configuration is a list of servers to connect to.
newtype IceConfig = IceConfig [Server]
  deriving (Eq, Show)

instance FromJSON IceConfig where
  parseJSON (Array v) = IceConfig <$> parseJSON (Array v)
  parseJSON (Object v) = IceConfig <$> v .: "servers"
  parseJSON _ = mzero

-- |Information on connecting to a server
data Server =
  Server {srvNick :: Text
         ,srvUsername :: Text
         ,srvPassword :: Password
         ,srvChannels :: [Text]
         ,srvId :: Text
         ,srvHostname :: HostName
         ,srvPort :: Integer
         ,srvLogFile :: FilePath}
  deriving (Eq,Show)


instance FromJSON Server where
  parseJSON (Object v) =
    do nick <- v .: "nick"
       hostname <- v .: "hostname"
       Server nick <$>
         v .:? "username" .!= nick <*>
         v .:? "password" .!= NoPassword <*>
         v .:? "channels" .!= [] <*>
         v .:? "id" .!= (pack hostname) <*>
         pure hostname <*>
         v .:? "port" .!= 6667 <*>
         v .:? "log-file" .!= "/dev/null"
  parseJSON _ = mzero

instance ToJSON Server where
  toJSON (Server n u p c i h o l) =
    object ["nick" .= n
           ,"username" .= u
           ,"password" .= p
           ,"channels" .= c
           ,"id" .= i
           ,"hostname" .= h
           ,"port" .= o
           ,"log-file" .= l]

-- |A password can either be demanded from stdin, omitted, or encoded
data Password = PromptPassword
              | NoPassword
              | NickServPassword Text
  deriving (Eq, Show)

instance FromJSON Password where
  parseJSON (String "prompt") = return PromptPassword
  parseJSON (String "stdin") = return PromptPassword
  parseJSON (String s) = return $ NickServPassword s
  parseJSON Null = return NoPassword
  parseJSON _ = mzero

instance ToJSON Password where
  toJSON PromptPassword = String "prompt"
  toJSON NoPassword = Null
  toJSON (NickServPassword s) = String s

-- | > fromString "prompt" = PromptPassword
-- > fromString "stdin" = PromptPassword
-- > fromString "" = NoPassword
-- > fromString s = NickServPassword $ pack s
instance IsString Password where
  fromString "prompt" = PromptPassword
  fromString "stdin" = PromptPassword
  fromString "" = NoPassword
  fromString s = NickServPassword $ pack s

-- |Try to read a file containing an 'IceConfig'. This is
-- exception-safe, in that all exceptions are caught in the
-- 'Exceptional' monad, rather than the 'IO' monad.
readConfigFile :: FilePath -> IO (Exceptional IceConfig)
readConfigFile fp =
  do decoded <- exceptIO $ decodeFileEither fp
     return $ case decoded of
                Failure x -> Failure x
                Success (Left e) -> Failure $ prettyPrintParseException e
                Success (Right e) -> Success e

-- |Read icebot.yaml from the current directory, try to marshal it into
-- an 'IceConfig'.
readConfig :: IO (Exceptional IceConfig)
readConfig = do path <- exceptIO $ makeAbsolute "icebot.yaml"
                case path of
                  Failure err -> return $ Failure err
                  Success pth -> readConfigFile pth

-- |Connect to a server.
runServer :: Server -> IO ThreadId
runServer srv =
  do pwd <-
       case srvPassword srv of
         NoPassword -> return Nothing
         PromptPassword ->
           do hSetEcho stdin False
              T.putStr (mconcat ["Enter password for ",(srvId srv),": "])
              pw <- T.getLine
              hSetEcho stdin True
              T.putStrLn ""
              return (Just pw)
         NickServPassword pw -> return (Just pw)
     forkIO $
       do hdl <-
            connectTo (srvHostname srv)
                      (PortNumber (fromInteger (srvPort srv)))
          hSetBinaryMode hdl True
          hSetBuffering hdl NoBuffering
          writeHdl hdl $ mappend "NICK " (srvNick srv)
          writeHdl hdl $ mconcat ["USER ",srvUsername srv," 0 * :icebot!"]
          forM_ (srvChannels srv) $
            \chan ->
              writeHdl hdl $ mappend "JOIN " chan
          case pwd of
            Nothing -> return ()
            Just pw ->
              writeHdl hdl $
              mconcat ["PRIVMSG NickServ :IDENTIFY ",srvNick srv," ",pw]
          listenHdl hdl
  where writeHdl hdl txt =
          do T.hPutStr hdl (mappend txt "\r\n")
             T.appendFile (srvLogFile srv)
                          (mconcat ["> ",txt,"\n"])
        listenHdl hdl =
          whileIo (hIsOpen hdl) $
          do line <-
               T.takeWhile (/= '\r') <$> T.hGetLine hdl
             case parseOnly (commandsParser <* endOfInput) line of
               Right x ->
                 case x of
                   PrivMsg chan _ cmd ->
                     case cmd of
                       Help ->
                         writeHdl hdl $ privMsg chan "No help for you!"
                       Source ->
                         writeHdl hdl $
                         privMsg chan "https://github.com/pharpend/icebot"
                       RobotRollCall ->
                         writeHdl hdl $
                         privMsg chan
                                 (mconcat [srvNick srv," reporting for duty!"])
                   Ping -> writeHdl hdl "PONG"
               Left x ->
                 T.appendFile (srvLogFile srv)
                              (mappend (pack (show x)) "\n")
             T.appendFile (srvLogFile srv)
                          (mconcat ["< ",line,"\n"])
        whileIo :: Monad m
                => m Bool -> m () -> m ()
        whileIo b x =
          do continue <- b
             if continue
                then x >> whileIo b x
                else return ()

-- |Private message someone
privMsg :: Channel -> Text -> Text
privMsg chan txt =
  mconcat ["PRIVMSG ",chan," :",txt,"\r\n"]

-- |The parser for commands
commandsParser :: Parser Message
commandsParser =
  pingParser <|>
  (do char ':'
      person <- takeWhile1 (/= '!')
      takeWhile1 (/= ' ')
      space
      privMsgParser person)

-- |Parse channel messages
privMsgParser :: Person -> Parser Message
privMsgParser p =
  do string "PRIVMSG"
     space
     chanName <- takeWhile1 (/= ' ')
     space
     char ':'
     cmd <-
       (
        do string "!robotrollcall"
           return RobotRollCall) <|>
       (do char '~'
           manyTill anyChar space >>=
             \case
               "help" -> return Help
               "?" -> return Help
               "source" -> return Source
               "src" -> return Source
               "robotrollcall" ->
                 return RobotRollCall
               x ->
                 fail (mconcat ["",x," is not a command I recognize."]))
     return $ PrivMsg chanName p cmd

-- |A Command
data Message = PrivMsg Channel Person Command
             | Ping
  deriving Show

type Channel = Text
type Person = Text

-- |A specific command
data Command = Help
             | Source
             | RobotRollCall
  deriving Show


pingParser :: Parser Message
pingParser = do string "PING"
                return Ping
