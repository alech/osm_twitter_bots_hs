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

main = do
	configFile <- getConfigFilePath "config"
	updateFile <- getConfigFilePath "last_update"
	fileExists <- doesFileExist configFile
	if not fileExists then
		exitWithWarning
	else
		parseAndAct configFile updateFile

exitWithWarning :: IO ()
exitWithWarning = do
	hPutStrLn stderr "Missing config file (~/.changeset_bot.config)!"
	exitFailure

-- ~/.changeset_bot.config
getConfigFilePath :: String -> IO FilePath
getConfigFilePath extension = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".changeset_bot." ++ extension

parseAndAct :: FilePath -> FilePath -> IO ()
parseAndAct configFile updateFile = do
	currentTimeString <- getCurrentTimeString
	putStrLn currentTimeString
	configs <- parseConfigFile configFile
	(lastUpdateTime, changeSetsAlreadyPosted) <- getUpdatesFromFile updateFile
	mapM_ (\(oConf, tConf) -> getChangeSetsAndTweet oConf tConf lastUpdateTime changeSetsAlreadyPosted) configs
	return ()

getCurrentTimeString :: IO String
getCurrentTimeString = do
	time <- getCurrentTime
	return (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00" time)

getChangeSetsAndTweet :: BoundingBox -> TwitterBotConfig -> String -> [Integer] -> IO ()
getChangeSetsAndTweet bb twitterBotConfig time csAlreadyPosted = do
	let apiUrl = osmApiChangeSetUrl bb time
	changeSets <- getChangeSets apiUrl
	let relevantChangeSets = filter (\cs -> not (changeSetId cs `elem` csAlreadyPosted)) $ filter (\cs -> changeSetInsideBoundingBox cs bb) $ filter (not . open) changeSets
	putStrLn $ show relevantChangeSets
	return ()
	-- update last_update_file

getChangeSets :: String -> IO [OSMChangeSet]
getChangeSets url = do
	-- FIXME deal with errors
	curlResult <- curlGetString url []
	putStrLn $ url
	return (changeSetsFromXML $ parseXML $ snd curlResult)
