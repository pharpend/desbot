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
-- Module      : Network.IRC.Desbot.REPL
-- Description : A read-eval-print-loop for testing desbot commands 
--               locally.
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module uses GNU Readline, and therefore only works on POSIX
-- systems.

module Network.IRC.Desbot.REPL where

import Network.IRC.Desbot.Parser

import Control.Monad
import qualified Data.ByteString.Char8 as BO
import Data.Yaml
import System.Console.Readline
import System.Exit

-- |Run the repl. This only works on POSIX.
repl :: REPLConf -> IO ()
repl conf =
  do let prompt = replPrompt conf
     readline prompt >>=
       \case
         Just line ->
           do case runPrivateCommand (BO.pack line) nullBotState of
                Left err ->
                  do putStrLn "Error in parsing command line:"
                     print err
                     addHistory line
                     repl conf
                Right msg ->
                  do BO.putStrLn msg
                     addHistory line
                     repl conf
         Nothing ->
           do putStrLn "\nGoodbye!"
              exitSuccess


-- |REPL configuration
data REPLConf = REPLConf {replPrompt :: String
                         ,replName :: String}
  deriving (Eq, Show)

instance FromJSON REPLConf where
  parseJSON (Object v)  = REPLConf <$> v .:? "prompt" .!= ">>= "
                                   <*> v .:? "name" .!= "luser"
  parseJSON _ = mzero

-- |Default REPL configuration
-- 
-- > nullREPLConf = REPLConf ">>= " "luser"
nullREPLConf :: REPLConf
nullREPLConf = REPLConf ">>= " "luser"
