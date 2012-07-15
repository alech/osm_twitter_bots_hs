module Tweet
	(
		TwitterBotConfig(..),
		tweet
	) where

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Trans
import Data.Maybe

import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient

import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.PercentEncoding

data TwitterBotConfig = TwitterBotConfig { 
		consumerKey       :: String,
		consumerSecret    :: String,
		accessToken       :: String,
		accessTokenSecret :: String
	}
	deriving (Eq, Show)

tweetUrl = fromJust $ parseURL "https://api.twitter.com/1/statuses/update.json"

twitterToken :: String -> String -> [(String, String)]
twitterToken accessToken accessTokenSecret = [("oauth_token", accessToken),
                ("oauth_token_secret", accessTokenSecret)]

loadToken :: TwitterBotConfig -> OAuthMonadT IO ()
loadToken (TwitterBotConfig consumerKey consumerSecret accessToken accessTokenSecret) =
	(return (AccessToken (Application consumerKey consumerSecret OOB) (fromList $ twitterToken accessToken accessTokenSecret))) >>= putToken

urlEncode :: [(String,String)] -> L.ByteString
urlEncode = L.pack . init . concatMap (\(k,v) -> encode k ++ "=" ++ encode v ++ "&")

tokenFromConfig :: TwitterBotConfig -> Token
tokenFromConfig (TwitterBotConfig consumerKey consumerSecret _ _) =
	fromApplication $ Application consumerKey consumerSecret OOB

-- tweets and returns whether the tweet was sent successfully
tweet :: TwitterBotConfig -> String -> IO Bool
tweet config statusText = do
	let token = tokenFromConfig config
	response <- runOAuthM token $ do
		loadToken config
		signRq2 HMACSHA1 Nothing tweetUrl { method     = POST,
                                            reqHeaders = fromList [("Content-Type", "application/x-www-form-urlencoded")],
                                            reqPayload = urlEncode [("status", statusText)]
                                           } >>= serviceRequest CurlClient
	return $ status response == 200
