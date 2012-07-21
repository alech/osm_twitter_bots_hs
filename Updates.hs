module Updates
	(getUpdatesFromFile,
	writeUpdatesToFile)
	where

import Data.List
import Data.Yaml.YamlLight
import Data.Maybe
import qualified Data.ByteString.Char8 as BC8

getUpdatesFromFile :: FilePath -> IO (String, [Integer])
getUpdatesFromFile filePath = do
	updates <- parseYamlFile filePath
	return $ parseUpdates updates

parseUpdates :: YamlLight -> (String, [Integer])
parseUpdates updates =
	(time, changeSetsPosted)
	where
		time = BC8.unpack $ fromJust $ unStr $ fromJust $ lookupYL (YStr $ BC8.pack "time") updates
		changeSetsPosted = map (read . BC8.unpack) $ mapMaybe unStr $ fromJust $ unSeq $ fromJust $ lookupYL (YStr $ BC8.pack "changesets_posted") updates

-- it looks like YamlLight can not produce YAML, so we do it by hand for now :-/
writeUpdatesToFile :: FilePath -> (String, [Integer]) -> IO ()
writeUpdatesToFile filePath (time, changeSetsPosted) = do
	writeFile filePath $ buildYaml time changeSetsPosted
	return ()

buildYaml :: String -> [Integer] -> String
buildYaml time changeSetsPosted =
	"---\n" ++ "time: " ++ time ++ "\n" ++ "changesets_posted:\n  - " ++ intercalate "\n  - " (map show changeSetsPosted) ++ "\n"
