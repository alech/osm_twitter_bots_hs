module Main where

import Tweet
import Config
import OSM

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)

main = do
	configFile <- getConfigFilePath
	fileExists <- doesFileExist configFile
	if not fileExists then
		exitFailure
	else
		parseAndAct configFile

-- ~/.changeset_bot.config
getConfigFilePath :: IO FilePath
getConfigFilePath = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".changeset_bot.config"

parseAndAct :: FilePath -> IO ()
parseAndAct configFile = do
	configs <- parseConfigFile configFile
	mapM_ (\(oConf, tConf) -> getChangeSetsAndTweet oConf tConf) configs

getChangeSetsAndTweet :: OSMConfig -> TwitterBotConfig -> IO ()
getChangeSetsAndTweet osmConfig twitterBotConfig = do
	let apiUrl = osmApiChangeSetUrl osmConfig
	putStrLn apiUrl

getChangeSets :: String -> IO [OSMChangeSet]
getChangeSets url = do
	-- get via curl, and pass to changeSetsFromXML
