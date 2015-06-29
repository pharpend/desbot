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
    -- * Configuration
  , Config(..)
  , Server(..)
  , Password(..)
  , BotConf(..)
  , nullBotConf
  , readConfig
  , readConfigFile
    -- * The REPL
  , repl
  , REPLConf(..)
  , nullREPLConf
    -- * Parsing IRC Messages
  , ircParser
  , parseIRC
  , Parser
  , BotState(..)
  , nullBotState
  , IRC(..)
  , Command(..)
  , AtWhom
  , FromWhom
  , Channel
  , pingParser
  , privmsgParser
  , NickOrChan(..)
  , nickOrChanParser
  , nickParser
  , chanParser
  , chanCommandParser
    -- **** NB: There is no @parseCommand@, because parsing a 'Command' requires a context.
  , runPrivateCommand
  , parsePrivateCommand
  , privateCommandParser
  , commandParser
  , runCommand
  , bugsParser
  , helpParser
  , licenseParser
  , manualParser
  , sourceParser
  )
  where

import Network.IRC.Desbot.Config
import Network.IRC.Desbot.Parser
import Network.IRC.Desbot.REPL

-- import Control.Concurrent
import Control.Exceptional
-- import Control.Monad (forM_)
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import Network
-- import System.IO

-- -- |Connect to a 'Server'. This forks the process into the background,
-- -- and returns the 'ThreadId' (from "Control.Concurrent").
-- runServer :: Server -> IO ThreadId
-- runServer srv =
--   do pwd <-
--        case srvPassword srv of
--          NoPassword -> return Nothing
--          PromptPassword ->
--            do hSetEcho stdin False
--               T.putStr (mconcat ["Enter password for ",(srvId srv),": "])
--               pw <- T.getLine
--               hSetEcho stdin True
--               T.putStrLn ""
--               return (Just pw)
--          NickServPassword pw -> return (Just pw)
--      forkIO $
--        do hdl <-
--             connectTo (srvHostname srv)
--                       (PortNumber (fromInteger (srvPort srv)))
--           hSetBinaryMode hdl True
--           hSetBuffering hdl NoBuffering
--           writeHdl hdl $ mappend "NICK " (srvNick srv)
--           writeHdl hdl $ mconcat ["USER ",srvUsername srv," 0 * :desbot!"]
--           forM_ (srvChannels srv) $
--             \chan ->
--               writeHdl hdl $ mappend "JOIN " chan
--           case pwd of
--             Nothing -> return ()
--             Just pw ->
--               writeHdl hdl $
--               mconcat ["PRIVMSG NickServ :IDENTIFY ",srvNick srv," ",pw]
--           listenHdl hdl
--   where writeHdl hdl txt =
--           do T.hPutStr hdl (mappend txt "\r\n")
--              T.appendFile (srvLogFile srv)
--                           (mconcat ["> ",txt,"\n"])
--         listenHdl hdl =
--           whileIo (hIsOpen hdl) $
--           do line <-
--                T.takeWhile (/= '\r') <$> T.hGetLine hdl
--              case parseOnly (ircParser <* endOfInput) line of
--                Right x ->
--                  case x of
--                    PrivMsg chan _ cmd ->
--                      case cmd of
--                        Help ->
--                          writeHdl hdl $ privMsg chan "No help for you!"
--                        Source ->
--                          writeHdl hdl $
--                          privMsg chan "https://github.com/pharpend/desbot"
--                        RobotRollCall ->
--                          writeHdl hdl $
--                          privMsg chan
--                                  (mconcat [srvNick srv," reporting for duty!"])
--                    Ping -> writeHdl hdl "PONG"
--                Left x ->
--                  T.appendFile (srvLogFile srv)
--                               (mappend (T.pack (show x)) "\n")
--              T.appendFile (srvLogFile srv)
--                           (mconcat ["< ",line,"\n"])
--         whileIo :: Monad m
--                 => m Bool -> m () -> m ()
--         whileIo b x =
--           do continue <- b
--              if continue
--                 then x >> whileIo b x
--                 else return ()

-- -- |Private message someone
-- privMsg :: Channel -> Text -> Text
-- privMsg chan txt =
--   mconcat ["PRIVMSG ",chan," :",txt,"\r\n"]
