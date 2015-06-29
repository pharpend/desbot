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
-- Module      : Main
-- Description : The Main module for desbot
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.IO as T
import Network.IRC.Desbot hiding (Manual, Parser)
import Paths_desbot
import Options.Applicative
import System.Directory
import System.IO
import System.Pager

main :: IO ()
main =
  do (WithConfigFile fp act) <- execParser desbotPI
     hSetBuffering stdout NoBuffering
     path <- makeAbsolute fp
     config <- runExceptional =<< readConfigFile path
     case act of
       Bot ->
         fail "You can't run the bot just yet"
       ConfigExample ->
         getDataFileName "res/config-example.yaml" >>= makeAbsolute >>=
         B.readFile >>=
         B8.putStr
       REPL -> repl (configREPL config)
       Manual ->
         getDataFileName "MANUAL.md" >>= T.readFile >>= printOrPage

data Args = WithConfigFile FilePath Action

data Action
  = REPL
  | ConfigExample
  | Manual
  | Bot

desbotPI :: ParserInfo Args
desbotPI =
  info (helper <*> desbotParser)
       (mconcat [fullDesc,progDesc "A useless IRC bot. For a manual see <https://github.com/pharpend/desbot/blob/master/MANUAL.md>."])

desbotParser :: Parser Args
desbotParser =
  WithConfigFile <$>
  strOption (mconcat [long "config-file"
                     ,short 'c'
                     ,metavar "PATH"
                     ,value "desbot.yaml"
                     ,help "The path to the configuration file."
                     ,showDefault]) <*>
  ((flag' ConfigExample
          (mconcat [long "config-example"
                   ,short 'e'
                   ,help "Show an example configuration file"])) <|>
   (flag' REPL
          (mconcat [long "repl"
                   ,long "interactive"
                   ,short 'i'
                   ,help "Run a REPL to test commands to desbot."])) <|>
   (flag' Manual
          (mconcat [long "manual"
                   ,long "man"
                   ,short 'm'
                   ,help "Show desbot's manual."])) <|>
   pure Bot)
