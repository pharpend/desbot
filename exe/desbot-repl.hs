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
-- Description : The desbot repl
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Network.IRC.Desbot hiding (Parser)
import Options.Applicative
import System.Directory
import System.IO

main :: IO ()
main =
  do args <- execParser desbotPI
     hSetBuffering stdout NoBuffering
     case args of
       WithConfigFile fp ->
         do Config replconf _ <- makeAbsolute fp >>= readConfigFile >>= runExceptional
            repl replconf

data Args = WithConfigFile FilePath

desbotPI :: ParserInfo Args
desbotPI =
  info (helper <*> desbotParser)
       (mconcat [fullDesc,progDesc "REPL to test commands for desbot"])

desbotParser :: Parser Args
desbotParser =
  (WithConfigFile <$>
   strOption (mconcat [long "config-file"
                      ,short 'c'
                      ,metavar "PATH"
                      ,value "desbot.yaml"
                      ,help "The path to the configuration file."
                      ,showDefault]))
