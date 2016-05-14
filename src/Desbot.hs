module Desbot where

import           Control.Lens
import           Data.Aeson
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Yaml
import           GHC.Generics
import           Network.IRC.Client
import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_desbot as P
import           System.IO.Unsafe
import           System.Process

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

-- |Read a file, parse it into a 'PrivateConf'
readPrivateConf :: FilePath -> IO PrivateConf
readPrivateConf fp =
  do res <- decodeFileEither fp
     either (fail . show) return res

-- |Connect to the configuration
connect'' :: PrivateConf -> IO ()
connect'' cfg =
  do c <- connCfg
     let instanceCfg = defaultIRCConf (cfg ^. nick)
         instanceCfg' =
           instanceCfg { _channels = cfg ^. autojoin
                       , _ctcpVer = mappend "desbot <https://github.com/pharpend/desbot>, "
                                            versionText
                       , _eventHandlers = mappend (_eventHandlers instanceCfg)
                                                  [evalHandler cfg]
                       }
     start c instanceCfg'
  where
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
          case _message e of
            Privmsg tgt (Right msg)
              -- Private message to the bot
              | tgt == cfg ^. nick -> mapM_ send (evald s Nothing msg)
              | otherwise -> return ()
            _ -> return ()
        Channel s nck ->
          case _message e of
            Privmsg tgt (Right msg)
              -- Message to a channel targeting the bot
              | or $ map (startsWith msg . mappend (cfg ^. nick)) [": ", ", "] ->
                  -- the nick plus ", " or ": "
                  let dbl = T.length (cfg ^. nick) + 2
                  in mapM_ send (evald s (Just nck) (T.drop dbl msg))
              | otherwise -> return ()
            _ -> return ()
        _ -> return ()
    x `startsWith` y = T.take (T.length y) x == y

-- |Thing that evaluates
--
-- Uses 'unsafePerformIO'
evald :: Text           -- ^Source
      -> Maybe Text     -- ^Person to target
      -> Text           -- ^Message
      -> [Message Text] -- ^Resulting messages. If there are multiple
                        -- lines in the result, they are represented as
                        -- separate messages.
evald src tgt x =
  unsafePerformIO $
    do result <- readProcess "mueval" ["-e", unpacked] mempty
       let -- Split into lines, ellipsize each line
           res = map (lineup . T.strip) $ T.lines $ T.pack result
           -- Ellipsize if there are more than 3 lines
           res' | 3 < length res = mappend res ["..."]
                | otherwise = res
       return $ map (Privmsg src . Right . applyTarget tgt) res'
  where
    -- Ellipsize if it's more than 64 characters
    lineup line | 64 > T.length line = line
                | otherwise = mappend (T.take 64 line) "..."
    unpacked = T.unpack x
    pmtr = Privmsg src . Right . applyTarget tgt . T.pack
    applyTarget tgt msg = case tgt of
                            Nothing -> msg
                            Just x -> mconcat [x, ": ", msg]


-- |The version of desbot
versionText :: Text
versionText = T.pack $(simpleVersion P.version)
