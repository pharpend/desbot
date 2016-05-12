{-# LANGUAGE LambdaCase #-}

-- desircBot - ircBot for #snowdrift on FreeNode
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
-- Module      : Network.IRC.Desbot.Bot
-- Description : The portion of desbot that talks to IRC servers
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Network.IRC.Desbot.Bot where

import Network.IRC.Desbot.Config
import Network.IRC.Desbot.Parser

import Control.Concurrent
import Control.Monad.State.Lazy
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network
import System.Console.Haskeline

-- |Take a 'Config', and use it to connect to all of the servers
-- concurrently. Returns the 'ThreadId's of the relevant threads.
-- 
-- > runConfig = fromConfig >=> traverse runIRCBot
runConfig :: Config -> IO [ThreadId]
runConfig = fromConfig >=> traverse runIRCBot

-- |Run a bot.
runIRCBot :: IRCBot -> IO ThreadId
runIRCBot = undefined

-- |Take a 'Config', marshal it into a series of 'IRCBot's. The IO part is
-- for the case of 'PromptPassword'.
fromConfig :: Config -> IO [IRCBot]
fromConfig cfg =
  for (configServers cfg) $
  \srv ->
    do nick <- pickNick srv
       let username =
             case srvUsername srv of
               Left _ -> nick
               Right txt -> T.encodeUtf8 txt
       password <-
         case srvPassword srv of
           NoPassword -> pure Nothing
           NickServPassword txt ->
             pure (Just (T.encodeUtf8 txt))
           PromptPassword ->
             fmap Just (promptPassword (T.unpack (srvId srv)))
       return IRCBot {ircBotConnectTo =
                        (srvHostname srv,PortNumber (fromInteger (srvPort srv)))
                     ,ircBotLogFile = srvLogFile srv
                     ,ircBotChannels =
                        fmap T.encodeUtf8 (srvChannels srv)
                     ,ircBotNick = nick
                     ,ircBotUsername = username
                     ,ircBotPassword = password
                     ,ircBotFunction = ircFunction}
  where pickNick srv =
          case srvNick srv of
            Defer ->
              case bcNicks (configBot cfg) of
                [] ->
                  fail "You cannot have an empty list of nicks."
                (x:_) -> return (T.encodeUtf8 x)
            Prefer [] ->
              fail "You cannot have an empty list of nicks."
            Prefer (x:_) ->
              return (T.encodeUtf8 x)
        promptPassword sid =
          runInputT defaultSettings
                    (getPassword
                       (Just '*')
                       (mconcat ["Please enter the password for ",sid,": "])) >>=
          \case
            Nothing ->
              do putStrLn "Please enter a password."
                 promptPassword sid
            Just p -> return (B8.pack p)

-- |An actual ircBot. It connects to exactly one server, has exactly one
-- nick, but connects to many channels.
data IRCBot =
  IRCBot {ircBotConnectTo :: (HostName,PortID)
         ,ircBotLogFile :: FilePath
         ,ircBotChannels :: [ByteString]
         ,ircBotNick :: ByteString
         ,ircBotUsername :: ByteString
         ,ircBotPassword :: Maybe ByteString
         ,ircBotFunction :: ByteString -> StateT BotState IO ByteString}
