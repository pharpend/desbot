module Desbot where

import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Yaml
import           GHC.Generics
import           Network.IRC.Client
import           Options.Applicative.Simple
import qualified Paths_desbot as P

data Config = Config { dcHost :: Text
                     , dcPort :: Int
                     , dcTls :: Bool
                     , dcLogFile :: Maybe FilePath
                     , dcAutojoin :: [Text]
                     , dcNick :: Text
                     , dcPass :: Text
                     }
  deriving (Eq, Show, Generic)     

instance ToJSON Config
instance FromJSON Config

makeLensesWith abbreviatedFields ''Config

-- |The version, for CTCP version
versionTxt :: Text
versionTxt = T.pack $(simpleVersion P.version)

-- |Read @config/desbot.yaml@, parse it into a 'Config'
readConfig :: IO Config
readConfig =
  do res <- decodeFileEither "config/desbot.yaml"
     either (fail . show) return res

-- |Connect to the configuration
connect'' :: Config -> IO ()
connect'' cfg =
  do let connCmd | cfg ^. tls = connectWithTLS'
                 | otherwise = connect'
         logger = case cfg ^. logFile of
                    Nothing -> stdoutLogger
                    Just fp -> fileLogger fp
                  
     connCfg <- connCmd logger
                        (T.encodeUtf8 (cfg ^. host))
                        (cfg ^. port)
                        1
     let instanceCfg = defaultIRCConf (cfg ^. nick)
         instanceCfg' =
           instanceCfg { _channels = cfg ^. autojoin
                       , _ctcpVer = mappend "desbot, " versionTxt
                       , _eventHandlers = mappend [echoH cfg]
                                                  (_eventHandlers instanceCfg)
                       }
           
           
     start connCfg instanceCfg'

echoH :: Config -> EventHandler ()
echoH cfg =
  EventHandler "Echo"
                EPrivmsg
                echoH'
  where  
    echoH' evt =
      case _message evt of
        Privmsg tgt whatever
          | tgt == cfg ^. nick ->
              case _source evt of
                User t -> send (Privmsg t whatever)
                _ -> return ()
          | otherwise -> return ()
        _ -> return ()
          
