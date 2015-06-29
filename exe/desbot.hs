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
-- Module      : Main
-- Description : The Main module for desbot
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Control.Applicative
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.IRC.Desbot hiding (Manual, Parser)
import Paths_desbot
import Options.Applicative
import Options.Applicative.Internal
import System.Directory
import System.Pager

main :: IO ()
main =
  do Args fp action' <- execParser desbotPI
     config <-
       case fp of
         Just fp -> runExceptional =<< readConfigFile =<< makeAbsolute fp
         Nothing ->
           do dataDir <-
                makeAbsolute =<< getAppUserDataDirectory "desbot"
              dataDirExists <- doesDirectoryExist dataDir
              if dataDirExists
                 then return ()
                 else do putStrLn (mappend "Creating default desbot configuration directory: "
                                           dataDir)
                         createDirectory dataDir
              let desbotYamlPath =
                    mappend dataDir "/desbot.yaml"
              desbotYamlExists <- doesFileExist desbotYamlPath
              if desbotYamlExists
                 then return ()
                 else do putStrLn (mappend "Adding default desbot configuration file to "
                                           desbotYamlPath)
                         defaultConfPath <-
                           getDataFileName "res/config-default.yaml"
                         defaultConf <- B.readFile defaultConfPath
                         B.writeFile desbotYamlPath defaultConf
                         putStrLn "You may want to edit the configuration file, but you are not required to."
              runExceptional =<< readConfigFile desbotYamlPath
     case action' of
       Bot ->
         fail "You can't run the bot just yet"
       ConfigExample ->
         getDataFileName "res/config-default.yaml" >>= makeAbsolute >>=
         B.readFile >>=
         B8.putStr
       REPL -> repl (configREPL config)
       Manual ->
         getDataFileName "MANUAL.md" >>= T.readFile >>= printOrPage

data Args = Args (Maybe FilePath) Action

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
  alt [Args <$> customConfigParser <*> actionParser]
  where alt [] = empty
        alt (x:xs) = x <|> alt xs

actionParser :: Parser Action
actionParser =
  subparser $
  mconcat [command "config-example"
                   (info (pure ConfigExample)
                         (mconcat [fullDesc
                                  ,progDesc "Show an example configuration file"]))
          ,command "repl"
                   (info (pure REPL)
                         (mconcat [fullDesc
                                  ,progDesc "Run a REPL to test commands to desbot."]))
          ,command "manual"
                   (info (pure Manual)
                         (mconcat [fullDesc,progDesc "Show desbot's manual."]))
          ,command "run"
                   (info (pure Bot)
                         (mconcat [fullDesc,progDesc "Run the actual bot"]))]

customConfigParser :: Parser (Maybe FilePath)
customConfigParser =
  option optionalStr
         (mconcat [long "config-file"
                  ,short 'c'
                  ,metavar "PATH"
                  ,value Nothing
                  ,help "The path to the configuration file."])


optionalStr :: ReadM (Maybe String)
optionalStr = eitherReader readString
  where readString s
          | T.null (T.strip (T.pack s)) = Right Nothing
          | otherwise = Right (Just s)
