module Config
	(
	parseConfigFile,
	BoundingBox(..),
	osmApiChangeSetUrl
	)
	where

import Tweet
import OSM
import Data.Yaml.YamlLight
import Data.Maybe
import qualified Data.ByteString.Char8 as BC8 -- TODO look into language feature
import Data.List (intercalate)

-- takes a filename and parses the file and returns a list of BoundingBoxes
-- and TwitterBotConfigs representing one bot
parseConfigFile :: FilePath -> IO [(BoundingBox, TwitterBotConfig)]
parseConfigFile filename = do
	yamlConfig <- parseYamlFile filename
	return (map configFromMap (fromJust $ unSeq yamlConfig))

-- parse config from one single entry
configFromMap :: YamlLight -> (BoundingBox, TwitterBotConfig)
configFromMap yl =
	(osmConfig, twitterBotConfig)
	where lookupKey yl key = BC8.unpack $ fromJust $ unStr $ fromJust $ lookupYL (YStr $ BC8.pack key) yl
	      osmConfig = BoundingBox minLat minLon maxLon maxLat
	      	where minLat = read $ lookupKey yl "min_lat" :: Double
	      	      minLon = read $ lookupKey yl "min_lon" :: Double
	      	      maxLat = read $ lookupKey yl "max_lat" :: Double
	      	      maxLon = read $ lookupKey yl "max_lon" :: Double
	      twitterBotConfig = TwitterBotConfig consumerKey consumerSecret accessToken accessTokenSecret
	      	where consumerKey       = lookupKey yl "consumer_key"
	      	      consumerSecret    = lookupKey yl "consumer_secret"
	      	      accessToken       = lookupKey yl "access_token"
	      	      accessTokenSecret = lookupKey yl "access_token_secret"

-- construct changeset API URL
osmApiChangeSetUrl :: BoundingBox -> String -> String
osmApiChangeSetUrl (BoundingBox minLat minLon maxLat maxLon) time =
	"http://api.openstreetmap.org/api/0.6/changesets?bbox=" ++ csv [minLon, minLat, maxLon, maxLat] ++ "&time=" ++ time
	where csv = (intercalate ",") . (map show)
