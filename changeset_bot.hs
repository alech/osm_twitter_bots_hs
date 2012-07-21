module Main where

import Tweet
import Config
import OSM

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main = do
	configFile <- getConfigFilePath
	fileExists <- doesFileExist configFile
	if not fileExists then
		exitWithWarning
	else
		parseAndAct configFile

exitWithWarning :: IO ()
exitWithWarning = do
	hPutStrLn stderr "Missing config file (~/.changeset_bot.config)!"
	exitFailure

-- ~/.changeset_bot.config
getConfigFilePath :: IO FilePath
getConfigFilePath = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".changeset_bot.config"

parseAndAct :: FilePath -> IO ()
parseAndAct configFile = do
	configs <- parseConfigFile configFile
	mapM_ (\(oConf, tConf) -> getChangeSetsAndTweet oConf tConf) configs
	return ()

getChangeSetsAndTweet :: BoundingBox -> TwitterBotConfig -> IO ()
getChangeSetsAndTweet bb twitterBotConfig = do
	let apiUrl = osmApiChangeSetUrl bb
	putStrLn apiUrl

getChangeSets :: String -> IO [OSMChangeSet]
getChangeSets url = do
	-- get via curl, and pass to changeSetsFromXML
	return []
