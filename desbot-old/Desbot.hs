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
-- Module      : Network.IRC.Desbot
-- Description : Umbrella module for the desbot library
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module is an "umbrella module", in that you don't need to import
-- any sub-modules of "Network.IRC.Desbot", because all of their
-- functions are exported by this module.
-- 
-- Note that the modules themselves are not re-exported. This way, you
-- don't have to read the documentation of five different modules, and
-- stitch them all together, you can just read the documentation for
-- this module.

module Network.IRC.Desbot
  ( -- ** Convenience re-exports
    module Control.Exceptional
    -- * Talking to IRC servers
  , runConfig
  , runIRCBot
  , fromConfig
  , IRCBot(..)
    -- * Configuration
    -- ** Porcelain functions
  , readConfig
  , readConfigFile
    -- ** Types
  , Config(..)
  , Server(..)
  , ServerNick(..)
  , Password(..)
  , BotConf(..)
  , nullBotConf
  , REPLConf(..)
  , nullREPLConf
    -- * The REPL
  , repl
    -- * Parsing IRC Messages
    -- ** Porcelain functions
  , runPrivateCommand
  , parsePrivateCommand
  , runIRC
  , parseIRC
  , fromBotConf
    -- ** Types
  , Parser
  , BotState(..)
  , nullBotState
  , IRC(..)
  , Command(..)
  , AtWhom
  , FromWhom
  , Channel
  , NickOrChan(..)
  -- ** Internal functions
  , ircParser
  , pingParser
  , privmsgParser
  , privateCommandParser
  , nickOrChanParser
  , nickParser
  , chanParser
  , chanCommandParser
  , commandParser
  , runCommand
  , bugsParser
  , helpParser
  , licenseParser
  , manualParser
  , sourceParser
  )
  where

import Network.IRC.Desbot.Bot
import Network.IRC.Desbot.Config
import Network.IRC.Desbot.Parser
import Network.IRC.Desbot.REPL

import Control.Exceptional
