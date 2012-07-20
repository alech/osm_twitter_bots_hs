module OSM (
		OSMChangeSet(..),
		changeSetsFromXML
	)
	where

import Text.XML.Light
import Data.Maybe (mapMaybe, fromJust)

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
	comment     :: String
} deriving (Show, Eq)

-- Text.XML.Light helper functions
qN :: String -> QName
qN qn = QName qn Nothing Nothing

-- use only on attributes you know to exist (otherwise fromJust fails)
attr :: String -> Element -> String
attr key =
	fromJust . findAttr (QName key Nothing Nothing)

doubleAttr :: String -> Element -> Double
doubleAttr key elem = read $ attr key elem :: Double

-- extract the comment value from a changeset element, given the following format
-- <changeset ...>
--     <tag k="comment" v="Modified via wheelmap.org"/>
-- </changeset>
extractComment :: Element -> String
extractComment changesetElem =
	concat $ mapMaybe extractValue $ filterElements isCommentKey changesetElem
	where isCommentKey e = (findAttr (qN "k") e) == Just "comment"
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
	OSMChangeSet csId user open bb comment
	where csId = read $ attr "id" changesetElem :: Integer
	      user = attr "user" changesetElem
	      open = "true" == attr "open" changesetElem
	      comment = extractComment changesetElem
	      bb   = BoundingBox minLat minLon maxLat maxLon
	      	where minLat = doubleAttr "min_lat" changesetElem
	      	      minLon = doubleAttr "min_lon" changesetElem
	      	      maxLat = doubleAttr "max_lat" changesetElem
	      	      maxLon = doubleAttr "max_lon" changesetElem