#! /usr/bin/env runhugs +l
--
-- Server.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--
{-# LANGUAGE RecordWildCards #-}
--module Server where

import ConcurrentUtils
import Control.Concurrent.Async
import Network
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import System.IO
import Text.Printf
import Control.Exception
import Data.Map  as Map

type ClientName = String
data Client = Client {
	name :: ClientName,
	handle :: Handle,
	kicked :: TVar Bool,
	chan :: TChan Message }

data Message = Notice String
	| Tell ClientName String
	| Broadcast ClientName String
	| Command String

data Server = Server {
	clients :: TVar (Map ClientName Client)}

newServer:: IO Server
newServer = do
	c <- newTVarIO Map.empty
	return Server { clients = c}


broadcast :: Server -> Message -> STM ()
broadcast Server{..} mes = do
	clientMap <- readTVar clients
	mapM_ (\cl -> sendMessage mes cl) (Map.elems clientMap)




newClient:: ClientName -> Handle -> STM Client
newClient n h = do
	c <- newTChan
	k <- newTVar False
	return Client { name = n,
		handle = h,
		kicked = k,
		chan = c}


sendMessage:: Message -> Client -> STM ()
sendMessage mes Client{..} =
	writeTChan chan mes 

checkAddClient:: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
	clientMap <- readTVar clients
	if Map.member name clientMap
	then return Nothing 
	else do client <- newClient name handle
		writeTVar clients $ Map.insert name client clientMap
		broadcast server $ Notice (name ++ " has connected") 
		return (Just client)

removeClient:: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
	modifyTVar' clients $ Map.delete name 
	broadcast server $ Notice (name ++ " has diconnected")


port:: Int
port = 8080

main :: IO()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))              
  server <- newServer
  printf "Listening on port %d\n" port
  forever $ do                                                  
     (handle, host, port) <- accept sock                         
     printf "Accepted connection from %s: %s\n" host (show port)
     forkFinally (talk handle server) (\_ -> hClose handle)
 

talk:: Handle -> Server -> IO ()
talk handle server@Server{..} = do
	hSetNewlineMode handle universalNewlineMode
	hSetBuffering handle LineBuffering
	readName
	where
	 readName = do
		hPutStrLn handle "What is your name?"
		name <- hGetLine handle
		if Prelude.null name
		  then readName
		  else mask $ \restore  -> do
		    ok <- checkAddClient server name handle	
		    case ok of
		      Nothing -> do
			hPrintf handle "The name is already in use!\n"
			readName
		      Just client@Client{..} ->
		 	restore (runClient server client) `finally` removeClient server name

runClient:: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do 
	race server receive
	return ()
	where
		receive = forever $ do
		  mes <- hGetLine handle 
		  atomically $ sendMessage  (Command mes) client 

		server = join $ atomically $ do
		  k <- readTVar kicked
		  case k of
		    True -> return $ 
			hPutStrLn handle $ "You have been kicked" 
		    False -> do
			mes <- readTChan chan 
			return $ do
				con <- handleMessage serv client mes	
				when con $ server


handleMessage:: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message = 
	case message of	
		Notice m 	-> out $ "***" ++ m
		Tell n m 	-> out $ "*" ++ n++ "* : " ++ m
		Broadcast n m 	-> out $ "<" ++ n++ ">:" ++m
		Command m -> 
			case words m of
				["/kick", who] -> do
					atomically $ kick server who name
					return True
				"/tell" :who: what -> do
					tell server client who (unwords what)
					return True
				["/quit"] ->
					return False
				('/':_):_ -> do
					hPutStrLn handle $ "not available" 
					return True
				_ -> do
					atomically $ broadcast server $ Broadcast name m
					return True

	where
	out s = do hPutStrLn handle s; return True


kick:: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
	cs <- readTVar clients
	case (Map.lookup who cs) of
		Nothing -> return () 	
		Just cl -> do
			let kick = kicked cl 
			writeTVar clients $ Map.delete who cs
			writeTVar kick True

tell:: Server -> Client -> ClientName -> String -> IO()
tell server@Server{..} client@Client{..} n mes = atomically $ do 
	cs <- readTVar clients
	case (Map.lookup n cs) of
		Nothing -> return ()
		Just cl -> sendMessage (Tell name mes) cl
	
	
