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
-- Module      : Network.IRC.Desbot.Config
-- Description : Configuration for Desbot
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the functions for loading & parsing desbot's
-- configuration files.

module Network.IRC.Desbot.Config where

import Network.IRC.Desbot.Parser hiding (Parser)

import Control.Applicative (Alternative(..))
import Control.Exceptional
import Control.Monad (ap, mzero)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml
import Network (HostName)
import System.Directory (makeAbsolute)

-- |Take a 'BotConf' and turn it into a 'BotState'. The only exception
-- this might throw is if 'bcNicks' returns '[]'.
fromBotConf :: BotConf -> Exceptional BotState
fromBotConf cfg =
  do nick <- case bcNicks cfg of
              [] -> fail "Nicks list mustn't be empty!"
              (x:_) -> pure x
     return (BotState {botNick = T.unpack nick
                      ,botSource = T.unpack (bcSourceUrl cfg)
                      ,botBugs = T.unpack (bcBugsUrl cfg)
                      ,botManual = T.unpack (bcManualUrl cfg)
                      ,botPrefix = T.unpack (bcPrefix cfg)})

-- |The configuration when running desbot, or desbot's REPL
data Config =
  Config {configBot :: BotConf
         ,configREPL :: REPLConf
         ,configServers :: [Server]}
  deriving (Eq,Show)

instance FromJSON Config where
  parseJSON (Array v) =
    Config <$> pure nullBotConf <*> pure nullREPLConf <*> parseJSON (Array v)
  parseJSON (Object v) =
    Config <$>
    (Just <$> (v .: "extra" <|> v .: "info") <|> pure Nothing) .!= nullBotConf <*>
    v .:? "repl" .!= nullREPLConf <*>
    v .: "servers"
  parseJSON _ = mzero

-- |Information about the bot in general
data BotConf =
  BotConf {bcNicks :: [Text]
          ,bcSourceUrl :: Text
          ,bcBugsUrl :: Text
          ,bcManualUrl :: Text
          ,bcPrefix :: Text}
  deriving (Eq,Show)

-- |The default 'BotConf'
nullBotConf :: BotConf
nullBotConf =
  BotConf [T.pack $ botNick nullBotState]
          (T.pack $ botSource nullBotState)
          (T.pack $ botBugs nullBotState)
          (T.pack $ botManual nullBotState)
          (T.pack $ botPrefix nullBotState)

instance FromJSON BotConf where
  parseJSON (Object v) =
    BotConf <$>
    withDefault
      (bcNicks nullBotConf)
      [fmap Just
            (alt [fmap (\x -> [x])
                       (attrOf v "nick")
                 ,attrOf v "nicks" >>=
                  \case
                    [] ->
                      fail "Nicks list may not be empty"
                    x -> return x])
      ,pure Nothing] <*>
    withDefault
      (bcSourceUrl nullBotConf)
      [fmap Just $ alt [attrOf v "source",attrOf v "source-url"],pure Nothing] <*>
    withDefault
      (bcBugsUrl nullBotConf)
      [fmap Just $ alt [attrOf v "bugs",attrOf v "bugs-url"],pure Nothing] <*>
    withDefault
      (bcManualUrl nullBotConf)
      [fmap Just $ alt [attrOf v "manual",attrOf v "manual-url"],pure Nothing] <*>
    withDefault
      (bcPrefix nullBotConf)
      [fmap Just $ alt [attrOf v "prefix",attrOf v "command-prefix"]
      ,pure Nothing]
    where alt [] = empty
          alt (x:xs) = x <|> alt xs
          withDefault x y = alt y .!= x
          attrOf :: FromJSON a
                 => Object -> Text -> Parser a
          attrOf = (.:)
  parseJSON Null = pure nullBotConf
  parseJSON _ = mzero

-- |REPL configuration
data REPLConf = REPLConf {replPrompt :: String
                         ,replName :: String}
  deriving (Eq, Show)

instance FromJSON REPLConf where
  parseJSON (Object v) =
    REPLConf <$> (v .:? "prompt" .!= ">>= ") <*> (v .:? "name" .!= "luser")
  parseJSON Null = pure nullREPLConf
  parseJSON _ = mzero

-- |Default REPL configuration
-- 
-- > nullREPLConf = REPLConf ">>= " "luser"
nullREPLConf :: REPLConf
nullREPLConf = REPLConf ">>= " "luser"

-- |Information on connecting to a server
data Server =
  Server {srvNick :: ServerNick
         ,srvUsername :: Either ServerNick Text
         ,srvPassword :: Password
         ,srvChannels :: [Text]
         ,srvId :: Text         -- ^This is an identifier used in
                                -- logging and error messages.
         ,srvHostname :: HostName
         ,srvPort :: Integer
         ,srvLogFile :: FilePath}
  deriving (Eq,Show)


instance FromJSON Server where
  parseJSON (Object v) =
    do nick <- v .: "nick"
       hostname <- v .: "hostname"
       Server nick `fmap`
         ((Right `fmap` (v .: "username")) <|> (pure (Left nick))) `ap`
         ((v .:? "password") .!= NoPassword) `ap`
         ((v .:? "channels") .!= []) `ap`
         ((v .:? "id") .!= (T.pack hostname)) `ap`
         (pure hostname) `ap`
         ((v .:? "port") .!= 6667) `ap`
         ((v .:? "log-file") .!= "/dev/null")
  parseJSON _ = mzero
  
-- |A transitive type for a nick in a 'Server'. 
data ServerNick = Defer
                | Prefer [Text]
  deriving (Eq, Show)

instance FromJSON ServerNick where
  parseJSON (Array l)
    | V.null l = pure Defer
    | otherwise = fmap (Prefer . V.toList) (traverse parseJSON l)
  parseJSON (String "") = pure Defer
  parseJSON Null = pure Defer
  parseJSON _ = mzero

--
-- |A password can either be demanded from stdin, omitted, or added via
-- 'NickServPassword'. Note that this is an instance of 'IsString', so
-- you can turn on @OverloadedStrings@, and enter plain strings instead
-- of wrapping them in a constructor.
-- 
-- See the documentation on the 'IsString' instance for more
-- information.
data Password = PromptPassword
              | NoPassword
              | NickServPassword Text
  deriving (Eq, Show)

instance FromJSON Password where
  parseJSON (String "prompt") = return PromptPassword
  parseJSON (String "stdin") = return PromptPassword
  parseJSON (String s) = return $ NickServPassword s
  parseJSON Null = return NoPassword
  parseJSON _ = mzero

-- | > fromString "prompt" = PromptPassword
-- > fromString "stdin" = PromptPassword
-- > fromString "" = NoPassword
-- > fromString s = NickServPassword $ T.pack s
instance IsString Password where
  fromString "prompt" = PromptPassword
  fromString "stdin" = PromptPassword
  fromString "" = NoPassword
  fromString s = NickServPassword $ T.pack s

-- |Try to read a file containing an 'Config'. This is
-- exception-safe, in that all exceptions are caught in the
-- 'Exceptional' monad, rather than the 'IO' monad.
readConfigFile :: FilePath -> IO (Exceptional Config)
readConfigFile fp =
  do decoded <- exceptIO $ decodeFileEither fp
     return $ case decoded of
                Failure x -> Failure x
                Success (Left e) -> Failure $ prettyPrintParseException e
                Success (Right e) -> Success e

-- |Read desbot.yaml from the current directory, try to marshal it into
-- an 'Config'.
readConfig :: IO (Exceptional Config)
readConfig = do path <- exceptIO $ makeAbsolute "desbot.yaml"
                case path of
                  Failure err -> return $ Failure err
                  Success pth -> readConfigFile pth
