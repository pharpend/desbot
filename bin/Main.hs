-- |Note: I copied a lot of this code from another project called
-- "tn". So some of the things are named @tnThis@ and @tnThat@. Changing
-- the variable names seemed more trouble than it was worth.
module Main where

import Desbot

import           Control.Lens
import qualified Data.ByteString as B
import           Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Options.Applicative
import           System.Directory

-- |Possible commands we can run
data Command = Run (Maybe FilePath) -- ^Run the bot, optionally with a path to the config file
             | Scaffold             -- ^Create necessary configuration files
             | ShowVersion          -- ^Print out the version
  deriving (Eq, Show)

-- |Main!
main :: IO ()
main =
  do result <- customExecParser tnPrefs (infoHelper tnParser tnInfo)
     case result of
       ShowVersion -> T.putStrLn versionText
       Scaffold -> 
         do c <- confDir
            mkdir_p c
            d <- dataDir
            mkdir_p d
            cfp <- confFilePath
            scfp <- sampleConfFilePath
            B.writeFile scfp sampleConfigText
            putStrLn (mconcat [ "You chould edit "
                              , scfp
                              , " now, and move it to "
                              , cfp
                              ])
       Run fp ->
         do cfp <- case fp of
                     Just p -> return p
                     Nothing -> confFilePath
            readPrivateConf cfp >>= runConfig
  where tnPrefs = prefs (mconcat [ disambiguate
                                 , showHelpOnError
                                 ])
        tnInfo = mconcat [ fullDesc
                         , progDesc "Haskell evalutation IRC bot"
                         ]
        -- The version command is special, because it's a top-level
        tnParser = versionCmd <|> tnParser'
        tnParser' = subconcat [ scaffoldCmd
                              , runCmd
                              ]
        subconcat = altConcat . fmap subparser
        x </> y = mconcat [x, "/", y]
        mkdir_p = createDirectoryIfMissing True
        sampleConfigText = $(embedFile "res/config.sample.yaml")
        confDir = getXdgDirectory XdgConfig "desbot"
        dataDir = getXdgDirectory XdgData "desbot"
        confFilePath = fmap (</> "config.yaml") confDir
        sampleConfFilePath = fmap (</> "config.sample.yaml") confDir
        runConfig conf =
          do -- We're changing the logFile if the user didn't specify one
             -- 
             -- It will look something like desbot-irc.freenode.net-20160404-213333.log
             -- 
             -- This is to prevent collisions.
             let nick' = T.unpack (conf ^. nick)
                 host' = T.unpack (conf ^. host)
             conf' <- case conf ^. logFile of
                        Just _ -> return conf
                        Nothing ->
                          do dp <- dataDir
                             currentTime <- getCurrentTime
                             let formattedTime = formatTime defaultTimeLocale
                                                            "%Y%m%d-%H%M%S"
                                                            currentTime
                                 logFileName = mconcat [ nick'
                                                       , "-"
                                                       , host'
                                                       , "-"
                                                       , formattedTime
                                                       , ".log"
                                                       ]
                                 finalFileName = dp </> logFileName
                             return (set logFile (Just finalFileName) conf)
             -- Finally, run the bot
             connect'' conf'

-- |The @desbot --version@ command.
versionCmd :: Parser Command
versionCmd =
  flag' ShowVersion
        (mconcat [ help "Show the version"
                 , long "version"
                 ])

-- |The @desbot scaffold@ command
scaffoldCmd :: Mod CommandFields Command
scaffoldCmd =
  command "scaffold"
          (infoHelper (pure Scaffold)
                      (mconcat [ fullDesc
                               , progDesc "Create necessary configuration files and directories."
                               ]))
  
-- |The @desbot run@ command
runCmd :: Mod CommandFields Command
runCmd =
  command "run"
          (infoHelper (Run <$> altConcat [ fmap Just configOption  
                                         , pure Nothing
                                         ])
                      (mconcat [ fullDesc
                               , progDesc "Actually run the bot."
                               ]))
  where configOption = strOption (mconcat [ long "config"
                                          , short 'c'
                                          , help "Path to a configuration file"
                                          ])

-- |Helper function that is for some reason not in
-- optparse-applicative. It just shortens typing a bit, so you don't
-- have to type
--
-- > info (helper <*> a) ...
--
-- Instead, it's just
--
-- > infoHelper a ...
infoHelper :: Parser a -> InfoMod a -> ParserInfo a
infoHelper a = info (helper <*> a)

-- |Sort of like 'mconcat' for 'Alternative's
altConcat :: (Foldable t, Alternative f) => t (f a) -> f a
altConcat = foldr (<|>) empty
