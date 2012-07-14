module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Tweet (TwitterBotConfig(..), tweet)

parseArgs :: [(String)] -> Maybe (String, String, String, String, String)
parseArgs args =
	if length args /= 5 then
		Nothing
	else
		Just (args !! 0, args !! 1, args !! 2, args !! 3, args !! 4)

bailWithErrorMessage :: String -> IO ()
bailWithErrorMessage msg = do 
	hPutStrLn stderr msg
	exitFailure

main = do
	-- parse command line arguments
	args <- getArgs
	let parsedArgs = parseArgs args
	case parsedArgs of
		Nothing -> bailWithErrorMessage "Incorrect number of parameters, need consumerkey, consumersecret, accesstoken, accesstokensecret and status!"
		_ -> return ()
	let (consumerKey, consumerSecret, accessToken, accessTokenSecret, status) = fromJust parsedArgs
	-- do actual tweeting
	response <- tweet (TwitterBotConfig consumerKey consumerSecret accessToken accessTokenSecret) status
	if response then putStrLn "Tweet succeeded" else putStrLn "Tweet failed"
