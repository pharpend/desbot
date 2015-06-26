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
-- Module      : Main
-- Description : The Main module for icebot
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Network
import Prelude hiding (log)
import System.IO
import System.Directory
import System.Exit

main :: IO ()
main = do fs <- freenodeSocket  
          runBot fs

freenodeSocket :: IO Handle
freenodeSocket = do hdl <- connectTo "irc.freenode.net" (PortNumber 6667)
                    hSetBinaryMode hdl True
                    hSetBuffering hdl NoBuffering
                    return hdl

initialCommands :: [ByteString]
initialCommands = ["NICK snowbot-test"
                  ,"USER snowbot 0 * :information"
                  ,"JOIN #snowdrift"]

writeCommand :: ByteString -> Handle -> IO ()
writeCommand bs hdl = do B.hPut hdl (mappend bs "\r\n")
                         log (mconcat ["> ", bs])

log :: ByteString -> IO ()
log line = do path <- makeAbsolute "freenode.log"
              B.appendFile path (mappend line "\n")

runBot :: Handle -> IO ()
runBot hdl = do forM_ initialCommands $
                  \c -> writeCommand c hdl
                forever $ B.hGetLine hdl >>=
                          log
