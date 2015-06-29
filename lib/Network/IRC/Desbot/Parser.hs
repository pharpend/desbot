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
--
-- The parser is based off of <https://tools.ietf.org/html/rfc2812 RFC
-- 2812>.

module Network.IRC.Desbot.Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Parsec

-- |Parse a 'String' into an 'IRC'. This is primarily for usage with
-- "Network.IRC.Desbot.REPL".
parseIRC :: ByteString       -- ^Input
         -> BotState
         -> Either ParseError IRC
parseIRC input bs =
  runParser ircParser bs "luser-input" input 

-- |Parse an IRC message
ircParser :: Parser IRC
ircParser = pingParser
            <|> privmsgParser
            <?> "Raw IRC command parser"

-- |A convenient alias, since all of the parsers are of this type.
type Parser = Parsec ByteString BotState

-- |The state of the bot. This just contains extra information like the
-- bot's nick.
data BotState = BotState {botNick :: String
                         ,botSource :: String
                         ,botBugs :: String
                         ,botManual :: String}
   deriving (Eq, Show)

-- |Default bot state
-- 
-- > nullBotState =
-- >   BotState "desbot"
-- >            "https://github.com/pharpend/desbot"
-- >            "https://github.com/pharpend/desbot/issues"
-- >            "https://github.com/pharpend/desbot/tree/master/MANUAL.md"
nullBotState :: BotState
nullBotState =
  BotState "desbot"
           "https://github.com/pharpend/desbot"
           "https://github.com/pharpend/desbot/issues"
           "https://github.com/pharpend/desbot/tree/master/MANUAL.md"

-- |An IRC command according to RFC 2812.
--
-- There is some semantic separation. For instance, messages to a
-- channel and messages to a person are represented separately here, but
-- are the same in the protocol.
data IRC = ChanMsg Channel (Maybe AtWhom) Command
         | PrivMsg FromWhom Command
         | Ping String
  deriving (Eq, Show)

-- |Alias for 'String'
type AtWhom = String
-- |Alias for 'String'
type FromWhom = String
-- |Alias for 'String'
type Channel = String

-- |A list of commands that can be requested
data Command = Bugs
             | Help
             | License
             | Manual
             | Source
  deriving (Eq, Show)

-- |Parser for @PING@s
pingParser :: Parser IRC
pingParser = do string "PING"
                Ping <$> many1 anyChar

-- |This parses an entire raw IRC line, tries to marshal it into either a
-- 'ChanMsg' or a 'PrivMsg'.
privmsgParser :: Parser IRC
privmsgParser =
  do char ':'
     sender <- nickParser
     manyTill anyChar space
     space
     string "PRIVMSG "
     nickOrChanParser >>=
       \case
         Chan chan ->
          do space
             char ':'
             (atWhom,cmd) <- chanCommandParser
             return $ ChanMsg chan atWhom cmd
         Nick _ ->
          do space
             char ':'
             cmd <- privateCommandParser
             return $ PrivMsg sender cmd

-- |This is used in the parsing of @PRIVMSG@s.
data NickOrChan = Nick String
                | Chan String

-- |Take a 'ByteString' and parse it into a 'NickOrChan'.
nickOrChanParser :: Parser NickOrChan
nickOrChanParser = (Nick <$> nickParser) <|>
                   (Chan <$> chanParser)

-- |Parses the name of a channel
--
-- From RFC 2812:
--
-- Channels names are strings (beginning with a '&', '#', '+' or '!'
-- character) of length up to fifty (50) characters.  Apart from the
-- requirement that the first character is either '&', '#', '+' or '!',
-- the only restriction on a channel name is that it SHALL NOT contain
-- any spaces (' '), a control G (^G or ASCII 7), a comma (',').  Space
-- is used as parameter separator and command is used as a list item
-- separator by the protocol).  A colon (':') can also be used as a
-- delimiter for the channel mask.  Channel names are case insensitive.
chanParser :: Parser String
chanParser = do firstChar <- oneOf allowedFirstChars
                rest <- manyTill anyChar (oneOf disallowedChars)
                return (firstChar : rest)
  where allowedFirstChars = "&#+!"
        disallowedChars = " \0x7,"

-- |Parses an IRC nick
nickParser :: Parser String
nickParser = do firstChar <- oneOf allowedFirstChars
                rest <- many (oneOf allowedExtraChars)
                return (firstChar : rest)
  where allowedFirstChars = mconcat [['A'..'Z']
                                    ,['a'..'z']
                                    ,"_\\[]{}^`|"]
        allowedExtraChars = mappend allowedFirstChars
                                    (mappend "-" ['0'..'9'])

-- |Parse a command posted in a channel
chanCommandParser :: Parser (Maybe AtWhom, Command)
chanCommandParser =
  do bs <- getState
     let ourNick = botNick bs
     atWhom <-
       do nick <-
            optionMaybe $
            do nick <- nickParser
               oneOf ":,"
               return nick
          many1 space
          case nick of
            Nothing -> pure Nothing
            Just n
              | n == ourNick -> pure Nothing
              | otherwise -> pure (Just n)
     char '~'
     cmd <- commandParser
     return (atWhom,cmd)

-- |Take a 'ByteString', try to parse it.
-- 
-- Primarily for use with the REPL
runPrivateCommand :: ByteString -- ^Input
                  -> BotState
                  -> Either ParseError ByteString
runPrivateCommand input bs =
  runCommand bs <$> parsePrivateCommand input bs

-- |Parse a command sent in a private message.
-- 
-- Primarily for use with the REPL
parsePrivateCommand :: ByteString -- ^Input
                    -> BotState
                    -> Either ParseError Command
parsePrivateCommand input bs =
  runParser privateCommandParser bs "luser-input" input

-- |Parse a command sent in a private message. Commands in private
-- messages don't need the '~' as a prefix, but it's not verboten.
privateCommandParser :: Parser Command
privateCommandParser =
  label (do optry $ do nickParser
                       oneOf ":,"
            optry $ many1 space
            optry $ char '~'
            commandParser)
        "a command."
  where optry = optional . try

-- |Take a 'Command', send back a 'ByteString'
runCommand :: BotState -> Command -> ByteString
runCommand bs Bugs = B8.pack (botBugs bs)
runCommand _ Help =
  T.encodeUtf8
    (T.intercalate
       "\r\n"
       ["I am desbot, the channel bot and dictator."
       ,"My full manual can be found with `~manual`."])
runCommand _ License = "https://gnu.org/licenses/agpl"
runCommand bs Manual = B8.pack (botManual bs)
runCommand bs Source = B8.pack (botSource bs)

-- |Parse a 'Command'
commandParser :: Parser Command
commandParser =
  bugsParser 
  <|> helpParser 
  <|> licenseParser
  <|> manualParser 
  <|> sourceParser 
  <?> "Parsing of your command"

-- |Parse the 'Bugs' command.
--
-- This succeeds on @~bugs@ or @~bug-reports@
bugsParser :: Parser Command
bugsParser =
  do trystr "bugs" <|> trystr "bug-reports"
     return Bugs

-- |Parse the 'Help' command.
--
-- This can be queried with either @~help@ or @~?@.
helpParser :: Parser Command
helpParser =
  do trystr "help" <|> trystr "?"
     return Help

-- |Parse the 'License' command.
--
-- This succeeds on @~license@ or @~terms@
licenseParser :: Parser Command
licenseParser =
  do trystr "license" <|> trystr "terms"
     return License

-- |Parse the 'Manual' command.
--
-- This succeeds on @~manual@ or @~man@
manualParser :: Parser Command
manualParser =
  do trystr "manual" <|> trystr "man"
     return Manual

-- |Parse the 'Source' command.
--
-- This succeeds on @~source@ or @~src@
sourceParser :: Parser Command
sourceParser =
  do trystr "source" <|> trystr "src"
     return Source

trystr :: String -> Parser String
trystr = try . string
