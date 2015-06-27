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

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.IRC.Icebot
import Paths_icebot
import Options.Applicative
import System.Directory
import System.IO

main :: IO ()
main =
  do args <- execParser icebotPI
     hSetBuffering stdout NoBuffering
     case args of
       ConfigExample ->
         getDataFileName "res/config-example.yaml" >>=
         makeAbsolute >>=
         B.readFile >>=
         B8.putStrLn
       WithConfigFile fp ->
         do path <- makeAbsolute fp
            IceConfig servers <- runExceptional =<< readConfigFile fp
            threads <- mapM runServer servers
            forM (zip threads servers) $
              \(tid,srv) ->
                T.putStrLn $
                mconcat ["Created thread with "
                        ,T.pack $ show tid
                        ," for server "
                        ,srvId srv
                        ,"."]
            _ <- getLine
            forM_ threads killThread

icebotPI :: ParserInfo Args
icebotPI =
  info (helper <*> icebotParser)
       (mconcat [fullDesc,progDesc "A useless IRC bot"])

icebotParser :: Parser Args
icebotParser =
  (WithConfigFile <$>
   strOption (mconcat [long "config-file"
                      ,short 'c'
                      ,metavar "PATH"
                      ,value "icebot.yaml"
                      ,help "The path to the configuration file."
                      ,showDefault])) <|>
  (flag' ConfigExample
         (mconcat [long "config-example"
                  ,short 'e'
                  ,help "Show an example configuration file"]))

data Args = WithConfigFile FilePath
          | ConfigExample
