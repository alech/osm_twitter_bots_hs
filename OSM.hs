module OSM (
		OSMChangeSet(..),
		BoundingBox(..),
		changeSetsFromXML,
		changeSetInsideBoundingBox,
		tweetFromChangeSet
	)
	where

import Text.XML.Light
import Data.Maybe (mapMaybe, fromJust)
import Data.List (inits)

data BoundingBox = BoundingBox {
	minLat      :: Double,
	minLon      :: Double,
	maxLat      :: Double,
	maxLon      :: Double
} deriving (Show, Eq)

-- only the parts relevant for us
data OSMChangeSet = OSMChangeSet {
	changeSetId :: Integer,
	user        :: String,
	open        :: Bool,
	boundingBox :: BoundingBox,
	comment     :: String,
	createdBy  :: String
} deriving (Show, Eq)

-- order by ID
instance Ord OSMChangeSet where
	(OSMChangeSet id1 _ _ _ _ _) `compare` (OSMChangeSet id2 _ _ _ _ _) = id1 `compare` id2

-- Text.XML.Light helper functions
qN :: String -> QName
qN qn = QName qn Nothing Nothing

-- use only on attributes you know to exist (otherwise fromJust fails)
attr :: String -> Element -> String
attr key =
	fromJust . findAttr (QName key Nothing Nothing)

doubleAttr :: String -> Element -> Double
doubleAttr key elem = read $ attr key elem :: Double

-- extract the tag value from a changeset element, given the following format
-- <changeset ...>
--     <tag k="key" v="value"/>
-- </changeset>
extractTagValue :: String -> Element -> String
extractTagValue key changesetElem =
	concat $ mapMaybe extractValue $ filterElements isCommentKey changesetElem
	where isCommentKey e = (findAttr (qN "k") e) == Just key
	      extractValue = findAttr (qN "v")

-- get a list of changesets from an OSM API reply
changeSetsFromXML :: [Content] -> [OSMChangeSet]
changeSetsFromXML xml =
	concatMap osmElementToChangeSets $ osmElements xml
	where osmElements xml = mapMaybe findOsmElem xmlElements
		where xmlElements = onlyElems xml
		      findOsmElem = findElement (qN "osm")

osmElementToChangeSets :: Element -> [OSMChangeSet]
osmElementToChangeSets osmElem =
	map changesetElementToChangeSet $ findChildren (qN "changeset") osmElem

-- FIXME refactor
changesetElementToChangeSet :: Element -> OSMChangeSet
changesetElementToChangeSet changesetElem = 
	OSMChangeSet csId user open bb comment createdBy
	where csId = read $ attr "id" changesetElem :: Integer
	      user = attr "user" changesetElem
	      open = "true" == attr "open" changesetElem
	      comment   = extractTagValue "comment" changesetElem
	      createdBy = extractTagValue "created_by" changesetElem
	      bb   = BoundingBox minLat minLon maxLat maxLon
	      	where minLat = doubleAttr "min_lat" changesetElem
	      	      minLon = doubleAttr "min_lon" changesetElem
	      	      maxLat = doubleAttr "max_lat" changesetElem
	      	      maxLon = doubleAttr "max_lon" changesetElem

-- checks whether a given changeset is inside a bounding box
changeSetInsideBoundingBox :: OSMChangeSet -> BoundingBox -> Bool
changeSetInsideBoundingBox (OSMChangeSet _ _ _ (BoundingBox minLat1 minLon1 maxLat1 maxLon1) _ _) (BoundingBox minLat2 minLon2 maxLat2 maxLon2) =
	(minLat1 > minLat2) && (minLon1 > minLon2) && (maxLat1 < maxLat2) && (maxLon1 < maxLon2)

-- length of shortened t.co URLs
-- a bit overestimated so we do not need to change it often
tCoShortenedUrlLength = 23

tweetFromChangeSet :: OSMChangeSet -> String
tweetFromChangeSet (OSMChangeSet csId user _ _ comment createdBy) = text ++ " http://www.openstreetmap.org/browse/changeset/" ++ (show csId)
	where text = head $ dropWhile (\t -> length t > (140 - tCoShortenedUrlLength)) options
	      options = [ commentOrNot ++ " by " ++ user ++ createdByOrNot,
	                  commentOrNot ++ " by " ++ user,
	                  commentOrNot ] ++ shortenedComments comment ++ [""]
	      	where
	      		shortenedComments comment = map (\e -> unwords e ++ " ...") $ reverse $ inits $ words comment
	      commentOrNot = if comment == "" then "(no comment)" else comment
	      createdByOrNot = if createdBy == "" then "" else " (" ++ createdBy ++ ")"
