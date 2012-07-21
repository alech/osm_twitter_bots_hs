module Main where

import Tweet
import Config
import OSM
import Updates

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import Text.XML.Light (parseXML)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.Curl (curlGetString)
import Control.Concurrent (threadDelay)

main = do
	configFile <- getConfigFilePath "config"
	updateFile <- getUpdateFilePath
	fileExists <- doesFileExist configFile
	if not fileExists then
		exitWithWarning
	else
		parseAndAct configFile updateFile

exitWithWarning :: IO ()
exitWithWarning = do
	hPutStrLn stderr "Missing config file (~/.changeset_bot.config)!"
	exitFailure

-- ~/.changeset_bot.[config|update]
getConfigFilePath :: String -> IO FilePath
getConfigFilePath extension = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".changeset_bot." ++ extension

getUpdateFilePath = do
	getConfigFilePath "last_update"

parseAndAct :: FilePath -> FilePath -> IO ()
parseAndAct configFile updateFile = do
	configs <- parseConfigFile configFile
	(lastUpdateTime, changeSetsAlreadyPosted) <- getUpdatesFromFile updateFile
	mapM_ (\(oConf, tConf) -> getChangeSetsAndTweet oConf tConf lastUpdateTime changeSetsAlreadyPosted) configs
	return ()

getCurrentTimeString :: IO String
getCurrentTimeString = do
	time <- getCurrentTime
	return (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00" time)

-- 'and' on functions. yeah!
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

getChangeSetsAndTweet :: BoundingBox -> TwitterBotConfig -> String -> [Integer] -> IO ()
getChangeSetsAndTweet bb twitterBotConfig time csAlreadyPosted = do
	currentTimeString <- getCurrentTimeString
	changeSets <- getChangeSets apiUrl
	mapM_ (tweetAndWriteUpdateFile twitterBotConfig currentTimeString) $ relevant changeSets
	return ()
	where apiUrl = osmApiChangeSetUrl bb time
	      relevant = filter (notAlreadyPosted .&&. insideBB .&&. (not . open))
	      	where notAlreadyPosted = (\cs -> not (changeSetId cs `elem` csAlreadyPosted))
	      	      insideBB = (\cs -> changeSetInsideBoundingBox cs bb)

tweetAndWriteUpdateFile :: TwitterBotConfig -> String -> OSMChangeSet -> IO ()
tweetAndWriteUpdateFile twitterBotConfig time cs = do
	tweetResult <- tweet twitterBotConfig $ tweetFromChangeSet cs
	updateFile <- getUpdateFilePath
	(_, changeSetsAlreadyPosted) <- getUpdatesFromFile updateFile
	if tweetResult then
		writeUpdatesToFile updateFile (time, (changeSetId cs):changeSetsAlreadyPosted)
	else
		return ()
	return ()

getChangeSets :: String -> IO [OSMChangeSet]
getChangeSets url = do
	-- FIXME deal with errors
	curlResult <- curlGetString url []
	return (changeSetsFromXML $ parseXML $ snd curlResult)
