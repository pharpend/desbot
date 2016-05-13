module Desbot where

import           Control.Lens
import           Data.Aeson
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Yaml
import           GHC.Generics
import           Language.Haskell.Interpreter
import           Network.IRC.Client
import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_desbot as P
import           System.IO.Unsafe

data PrivateConf = PrivateConf { pcHost :: Text
                               , pcPort :: Int
                               , pcTls :: Bool
                               , pcLogFile :: Maybe FilePath
                               , pcAutojoin :: [Text]
                               , pcNick :: Text
                               , pcPass :: Text
                               }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateConf
instance FromJSON PrivateConf

makeLensesWith abbreviatedFields ''PrivateConf

-- |Read @config/desbot.yaml@, parse it into a 'Conf'
readPrivateConf :: IO PrivateConf
readPrivateConf =
  do res <- decodeFileEither "config/desbot.yaml"
     either (fail . show) return res



-- |Connect to the configuration
connect'' :: PrivateConf -> IO ()
connect'' cfg =
  do c <- connCfg
     let instanceCfg = defaultIRCConf (cfg ^. nick)
         instanceCfg' =
           instanceCfg { _channels = cfg ^. autojoin
                       , _ctcpVer = mappend "desbot <https://github.com/pharpend/desbot>, "
                                            versionTxt
                       , _eventHandlers = mappend (_eventHandlers instanceCfg)
                                                  [evalHandler cfg]
                       }
     start c instanceCfg'
  where
    versionTxt = T.pack $(simpleVersion P.version)
    connCmd | cfg ^. tls = connectWithTLS'
            | otherwise = connect'
    logger =
      case cfg ^. logFile of
        Nothing -> stdoutLogger
        Just fp -> fileLogger fp
    connCfg = connCmd logger
                      (T.encodeUtf8 (cfg ^. host))
                      (cfg ^. port)
                      1

-- |Evaluate private messages sent directly to the bot
evalHandler :: PrivateConf -> EventHandler ()
evalHandler cfg =
  EventHandler "Haskell evaluation in a private message"
               EPrivmsg
               ef
  where
    ef e =
      -- The source has to be a user
      case _source e of
        User s ->
          -- It has to be a PRIVMSG to the bot
          case _message e of
            Privmsg tgt (Right msg)
              | tgt == cfg ^. nick -> mapM_ send (evald s msg)
              | otherwise -> return ()
            _ -> return ()
        _ -> return ()

-- |Thing that evaluates
--
-- Alright, fuck it, we're using unsafePerformIO
evald :: Text -> Text -> [Message Text]
evald tgt x =
  unsafePerformIO $
    do result <-
         runInterpreter $
           do setImports ["Prelude"]
              res <- eval unpacked
              return (T.pack res)
       return $ either show' (pure . Privmsg tgt . Right) result
  where
    unpacked = T.unpack x
    show' =
      \case
        UnknownError s -> [pmtr s]
        WontCompile errs ->
          mconcat $
            map (\s -> let s' = map pmtr (errMsg' s)
                       in if length s' <= 3
                            then s'
                            else take 2 s' ++ [pmtr "..."])
                errs
        NotAllowed s -> [pmtr s]
        GhcException s -> [pmtr s]
    pmtr s = Privmsg tgt (Right (T.pack s))
    errMsg' = lines . errMsg
