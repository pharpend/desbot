{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- desbot - bot for #snowdrift on FreeNode
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
-- Module      : Network.IRC.Desbot.Parser
-- Description : The parser for IRC commands
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains 'Parser's for incoming IRC commands.

module Network.IRC.Desbot.Parser where

import Control.Applicative (Alternative (..))
import Data.Text (Text)
import Data.Attoparsec.Text

-- |This parses a raw line from the IRC server, and marshals it into a
-- 'Message'.
ircParser :: Parser Message
ircParser =
  pingParser <|>
  (do char ':'
      person <- takeWhile1 (/= '!')
      takeWhile1 (/= ' ')
      space
      privMsgParser person)

-- |Parser for @PING@s
pingParser :: Parser Message
pingParser = do string "PING"
                return Ping

-- |This parses @PRIVMSG@s. That is, private messages sent to the bot,
-- as well as text typed in a channel in which the bot is.
privMsgParser :: Person -> Parser Message
privMsgParser p =
  do string "PRIVMSG"
     space
     chanName <- takeWhile1 (/= ' ')
     space
     char ':'
     char '~'
     cmd <- commandParser
     return $ PrivMsg chanName p cmd

-- |This is the same as 'privMsgParser', but it doesn't need the
-- @PRIVMSG person :@ before the message
commandParser :: Parser Command
commandParser =
  manyTill anyChar space >>=
  \case
    "help" -> return Help
    "?" -> return Help
    "source" -> return Source
    "src" -> return Source
    "robotrollcall" -> return RobotRollCall
    x ->
      fail (mconcat [x," is not a command I recognize."])

-- |This is a wrapper around 'commandParser'. It parses a line.
parseCommand :: Text -> Either String Command
parseCommand = parseOnly (commandParser <* endOfInput)

-- |Convert a 'Command' into a 'Text' response
runCommand :: Command -> Text
runCommand cmd =
  case cmd of
    Help -> "No help for you!"
    Source -> "https://github.com/pharpend/desbot"
    RobotRollCall -> "desbot reporting for duty!"


-- |At the moment, two types of messages are supported:
-- 
-- 'PrivMsg's are things said in a channel, or things messaged to the
-- bot.
-- 
-- 'Ping's are sent to desbot by the server.
data Message = PrivMsg Channel Person Command
             | Ping
  deriving (Eq, Show)

-- |Alias for 'Text'
type Channel = Text
-- |Alias for 'Text'
type Person = Text

-- |Commands sent to the bot in a 'PrivMsg'.
data Command = Help
             | Source
             | RobotRollCall
  deriving (Eq, Show)


