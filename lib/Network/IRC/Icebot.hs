{-# LANGUAGE LambdaCase #-}
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
-- Module      : Network.IRC.Icebot
-- Description : Umbrella module for the icebot library
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : AGPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Network.IRC.Icebot 
  ( -- ** Convenience re-exports
    module Control.Exceptional
    -- * Configuration
  , IceConfig
  , Server(..)
  , readConfig
  , readConfigFile
  )
  where

import Control.Exceptional
import Control.Monad (mzero)
import Control.Parallel
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Yaml
import Network
import System.Directory
import System.IO

-- |The Icebot configuration is a list of servers to connect to.
type IceConfig = [Server]

-- |Information on connecting to a server
data Server =
  Server {srvNick :: Text
         ,srvUsername :: Text
         ,srvPassword :: Password
         ,srvChannels :: [Text]
         ,srvHostname :: HostName
         ,srvPort :: Integer
         }
  deriving (Eq,Show)
 

instance FromJSON Server where
  parseJSON (Object v) = Server <$> v .: "nick"
                                <*> v .: "username"
                                <*> v .: "password"
                                <*> v .: "channels"
                                <*> v .: "hostname"
                                <*> v .: "port"
  parseJSON _ = mzero

instance ToJSON Server where
  toJSON (Server n u p c h o) =
    object ["nick" .= n
           ,"username" .= u
           ,"password" .= p
           ,"channels" .= c
           ,"hostname" .= h
           ,"port" .= o]

-- |A password can either be demanded from stdin, omitted, or encoded
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

instance ToJSON Password where
  toJSON PromptPassword = String "prompt"
  toJSON NoPassword = Null
  toJSON (NickServPassword s) = String s
  
-- | > fromString "prompt" = PromptPassword
-- > fromString "stdin" = PromptPassword
-- > fromString "" = NoPassword
-- > fromString s = NickServPassword $ pack s
instance IsString Password where
  fromString "prompt" = PromptPassword
  fromString "stdin" = PromptPassword
  fromString "" = NoPassword
  fromString s = NickServPassword $ pack s

-- |Try to read a file containing an 'IceConfig'. This is
-- exception-safe, in that all exceptions are caught in the
-- 'Exceptional' monad, rather than the 'IO' monad.
readConfigFile :: FilePath -> IO (Exceptional IceConfig)
readConfigFile fp = do decoded <- exceptIO $ decodeFileEither fp
                       return $ case decoded of
                                  Failure x -> Failure x
                                  Success (Left e) -> Failure $ prettyPrintParseException e
                                  Success (Right e) -> Success e

-- |Read icebot.yaml from the current directory, try to marshal it into
-- an 'IceConfig'.
readConfig :: IO (Exceptional IceConfig)
readConfig = do path <- exceptIO $ makeAbsolute "icebot.yaml"
                case path of
                  Failure err -> return $ Failure err
                  Success pth -> readConfigFile pth

-- |Given an 'IceConfig', connect to each of the servers, join the
-- channels in parallel, return one handle per server.
runIceConfig :: IceConfig -> IO [Handle]
runIceConfig [] = return []
runIceConfig (x:xs) =
  do handle <- connectTo (srvHostname x)
                         (PortNumber (fromInteger (srvPort x)))
     rest <- par handle (runIceConfig xs)
     return (handle : rest)
