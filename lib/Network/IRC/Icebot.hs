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
    -- * Running the server
  , Threads
  , runServer
  , repl
  , iceParse
  )
  where

import Control.Concurrent
import Control.Exceptional
import Control.Monad
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Yaml
import Network
import System.Console.Readline
import System.Directory
import System.IO
import Text.Parsec

-- |The Icebot configuration is a list of servers to connect to.
type IceConfig = [Server]

-- |Information on connecting to a server
data Server =
  Server {srvNick :: Text
         ,srvUsername :: Text
         ,srvPassword :: Password
         ,srvChannels :: [Text]
         ,srvId :: Text
         ,srvHostname :: HostName
         ,srvPort :: Integer
         ,srvLogFile :: FilePath}
  deriving (Eq,Show)


instance FromJSON Server where
  parseJSON (Object v) = Server <$> v .: "nick"
                                <*> v .:? "username" .!= v .: "nick"
                                <*> v .:? "password" .!= NoPassword
                                <*> v .:? "channels" .!= []
                                <*> v .:? "id" .!= v .: "hostname"
                                <*> v .: "hostname"
                                <*> v .:? "port" .!= 6667
                                <*> v .:? "log-file" .!= "/dev/null"
  parseJSON _ = mzero

instance ToJSON Server where
  toJSON (Server n u p c i h o l) =
    object ["nick" .= n
           ,"username" .= u
           ,"password" .= p
           ,"channels" .= c
           ,"id" .= i
           ,"hostname" .= h
           ,"port" .= o
           ,"log-file" .= l]

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
readConfigFile fp =
  do decoded <- exceptIO $ decodeFileEither fp
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

-- |Connect to a server.
runServer :: Server -> IO ()
runServer srv =
  do hdl <-
       connectTo (srvHostname srv)
                 (PortNumber (fromInteger (srvPort srv)))
     hSetBinaryMode hdl True
     hSetBuffering hdl NoBuffering
     writeHdl hdl $
       mappend "NICK " (srvNick srv)
     writeHdl hdl $
       mconcat ["USER ",srvUsername srv," 0 * :icebot!"]
     forM_ (srvChannels srv) $
       \chan ->
         writeHdl hdl $
         mappend "JOIN " chan
     case srvPassword srv of
       NoPassword -> return ()
       PromptPassword ->
         do pwd <- T.getLine
            writeHdl hdl
                     (mconcat ["PRIVMSG NickServ :IDENTIFY "
                              ,srvNick srv
                              ," "
                              ,pwd])
       NickServPassword pwd ->
         writeHdl hdl
                  (mconcat ["PRIVMSG NickServ :IDENTIFY ",srvNick srv," ",pwd])
  where writeHdl hdl txt =
          do T.hPutStr hdl (mappend txt "\r\n")
             T.appendFile (srvLogFile srv)
                          (mappend txt "\n")

type Threads = [(ThreadId, Server)]

-- |Run the read-eval-print loop
repl :: Threads -> IO ()
repl srvs =
  readline ">>= " >>=
  \case
    Nothing -> T.putStrLn "\nGoodbye!" 
    Just "quit" -> T.putStrLn "\nGoodbye!" 
    Just "exit" -> T.putStrLn "\nGoodbye!" 
    Just s ->
      do addHistory s
         iceParse srvs s
         repl srvs

iceParse :: Threads -> String -> IO ()
iceParse ts "" = return ()
iceParse ts command =
  case runParser iceParser () "keyboard input" (pack command) of
    Left err -> print err
    Right Help -> putStrLn "No help for you!"
    Right Threads ->
      forM_ ts $
      \(tid,srv) ->
        T.putStrLn (mconcat ["Server ",srvId srv," on ",pack $ show tid])

data Command = Help
             | Threads

iceParser :: Parsec Text () Command
iceParser = helpParser <|> threadsParser
  where helpParser = do try $ string "help"
                        return Help
        threadsParser = do try $ string "threads"
                           return Threads
