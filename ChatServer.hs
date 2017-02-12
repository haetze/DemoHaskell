#! /usr/bin/env runhugs +l
--
-- ChatServer.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE RecordWildCards #-}

--module ChatServer where


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



data Server = Server{
	rooms :: TVar (Map String Room)
}

data Room = Room {
	clients :: TVar (Map String Client)
}

data Client = Client {
	chan :: TChan Message,
	handl :: Handle,
	name :: String,
	roomName :: String
}

data Message = Command String
	| Notice String
	deriving (Show, Read, Eq)


newServer :: IO Server
newServer = do
	server <- newTVarIO Map.empty
	return Server{ rooms=server}


newClient :: String -> Handle -> String -> STM Client
newClient n h r = do
	cha <- newTChan
	return Client { chan = cha, handl = h, name = n, roomName = r }

sendMessage:: Client -> Message -> STM ()
sendMessage Client{..} m = writeTChan chan m


broadcast:: Room -> Message -> STM ()
broadcast Room{..} m = do
	room <- readTVar clients
	mapM_ (\client -> sendMessage client m) (Map.elems room)


newRoom:: IO Room
newRoom = do
	cl <- newTVarIO Map.empty
	return Room{ clients = cl} 

insertRoom :: Server -> Room -> String -> STM ()
insertRoom Server{..} room name = do
	roomList <- readTVar rooms 
	case (Map.lookup name roomList) of
	  Nothing -> writeTVar rooms $ Map.insert name room roomList
	  Just _ -> return ()


insertClientInRoom:: Client -> Room -> STM ()
insertClientInRoom client@Client{..} Room{..} = do
	cs <- readTVar clients
	case (Map.lookup name cs ) of
	  Nothing -> writeTVar clients $ Map.insert name client cs
	  Just _ -> return ()

removeClientFromRoom:: Client -> Room -> STM ()
removeClientFromRoom client@Client{..} Room{..} = do
	cs <- readTVar clients
	case (Map.lookup name cs ) of
	  Just _ -> writeTVar clients $ Map.delete name cs
	  Nothing -> return ()

receiveLoopClient:: Client -> IO ()
receiveLoopClient client@Client{..} = do
	m <- hGetLine handl	
	atomically $ sendMessage client (Command m)
	receiveLoopClient client

startClient :: Handle -> Server -> IO ()
startClient h  server@Server{..} = do 
	hSetNewlineMode h universalNewlineMode
	hSetBuffering h LineBuffering
	hPutStrLn h "Whats your name?"
	n <- hGetLine h
	hPutStrLn h "What room you want to join?"
	r <- hGetLine h
	mR <- newRoom
	me <- atomically $ newClient n h r
	atomically $ insertRoom server mR r
	a <- atomically $ insertClientInRoomOnServer r server me 
	case a of
	  True -> do
		forkFinally (receiveLoopClient me) (\_ -> hClose h)
		handleMessagesFromClient me server
	  False -> do
		hPutStrLn h "Name already taken" 
		return ()

insertClientInRoomOnServer :: String -> Server -> Client -> STM Bool
insertClientInRoomOnServer r server@Server{..} client = do
	roomList <-readTVar rooms 
	case (Map.lookup r roomList) of 
	  Nothing -> return False
	  Just room ->do 
	    t <- clientInRoom client room
	    case t of
	      False -> do 
		insertClientInRoom client room 
	      	let a = Map.delete r roomList
	    	let b = Map.insert r room a 
	    	writeTVar rooms b
		return True
	      True -> return False


clientInRoom:: Client -> Room -> STM Bool
clientInRoom client Room{..} = do
	cli <- readTVar clients
	return (Map.member (name client) cli)

clientInRoomOnServer :: Client -> Server -> String -> STM Bool
clientInRoomOnServer client server@Server{..} n = do
	r <- readTVar rooms 
	case (Map.lookup n r) of
	  Nothing -> return True
	  Just room -> clientInRoom client room


removeClientFromRoomOnServer :: Client -> String -> Server -> STM ()
removeClientFromRoomOnServer client r server@Server{..} = do
	roomList <-readTVar rooms 
	case (Map.lookup r roomList) of 
	  Nothing -> return ()
	  Just room ->do 
	    removeClientFromRoom client room 
	    let a = Map.delete r roomList
	    let b = Map.insert r room a 
	    writeTVar rooms b



	
handleMessagesFromClient:: Client -> Server-> IO ()
handleMessagesFromClient client@Client{..} server@Server{..} = do
	s <- newTVarIO client
	m <- atomically $ readTChan chan	
	case m of
	  Notice m -> hPutStrLn handl m
	  Command m -> do
		case (words m) of
		  "/changeRoom":to:[] -> atomically $ do
			a <- clientInRoomOnServer client server to
                        case a of
		  	  False -> do 
				removeClientFromRoomOnServer client roomName server
				insertClientInRoomOnServer to server client
				writeTVar s (changeRoomInClient client to) 
			  True -> sendMessageToSelf client 
		  _ -> atomically $ do
		 	sendMessageToRoomOnServer roomName server (Notice (name++":"++m))
	cl <- atomically $ readTVar s
	handleMessagesFromClient cl server

sendMessageToSelf :: Client -> STM ()
sendMessageToSelf client@Client{..} = do
	writeTChan chan (Notice "Name was already taken in this channel; you stayed in your old channel")

sendMessageToRoomOnServer :: String -> Server -> Message -> STM ()
sendMessageToRoomOnServer roomName server@Server{..} m = do
	roomList <- readTVar rooms
	case (Map.lookup roomName roomList) of
	  Nothing -> return ()
	  Just r -> broadcast r m

changeRoomInClient :: Client -> String -> Client
changeRoomInClient client room = Client{
	chan = (chan client),
	handl = (handl client),
	name = (name client),
	roomName = room}	
	
	
--port:: Int
port = 8080

main :: IO()
main = do
  sock <- listenOn (PortNumber port)              
  server <- newServer
  --printf "Listening on port %d\n" port
  forever $ do                                                  
     (handle, host, port) <- accept sock                         
     printf "Accepted connection from %s: %s\n" host (show port)
     forkIO $ startClient handle server 
 
