{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (forever, liftM)
import Data.List (find)
import Data.Maybe (catMaybes)
import Control.Concurrent (forkFinally)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Environment
import Control.Exception (catch, IOException)


data Method = GET | POST | PATCH | DELETE
    deriving (Eq, Show, Read)
type Target = String -- Not ByteString since we will pattern match on this.
type Version = BC.ByteString
type Header = (BC.ByteString, BC.ByteString)
type Body = BC.ByteString
data Message = Message (Method, Target, Version) [Header] Body

-- Unfortunate that we are not permitted to use a parsing library here.
parse :: BC.ByteString -> Maybe Message
parse msg = do
    let (reqString, rest) = B.breakSubstring "\r\n" msg
        (headersLine, body) = B.breakSubstring "\r\n\r\n" (BC.strip rest)
        headers = catMaybes . map parseHeader $ BC.split '\r' headersLine
        parseHeader h = case B.split 58 h of -- fromEnum ':' == 58
            [key, value] -> Just (BC.strip key, BC.strip value)
            _ -> Nothing
    (method, target, version) <- case  BC.split ' ' reqString of 
        [method, target, version] ->  Just (read . BC.unpack $ method, BC.unpack target, version)
        _ -> Nothing
    return $ Message (method, target, version) headers body

    



main :: IO ()
main = do
    args <- getArgs 
    
    hSetBuffering stdout LineBuffering

    let host = "127.0.0.1"
        port = "4221"
        -- Toy argument parsing for now.
        directory = case args of
            ["--directory", x] -> Just x
            _ -> Nothing
    
    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port
    
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    
    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5
    
    let respond :: Socket -> IO ()
        respond client = do 
            msgString <- recv client 1024
            res <- case parse msgString of 
                Nothing -> return $ badRequest
                Just msg -> route directory msg
            sendAll client res

    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        _ <- forkFinally (respond clientSocket) (\_ -> close clientSocket)
        return ()


route :: Maybe String -> Message -> IO BC.ByteString
route _ (Message (GET, "/", _) _ _) = return "HTTP/1.1 200 OK\r\n\r\n"

route _ (Message (GET, '/' : 'e' : 'c' : 'h' : 'o' : '/' : text, _) _ _) = return $ BC.pack . concat $ [
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ", 
    contentLength,
    "\r\n\r\n",
    text]
    where contentLength = show . length $ text

route _ (Message (GET, "/user-agent", _) headers _) = return $ case getHeader headers "User-Agent" of
        Just userAgent -> B.concat [
                           "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ",
                           contentLength,
                           "\r\n\r\n",
                           BC.strip userAgent]
            where contentLength = BC.pack . show . B.length . BC.strip $ userAgent
        Nothing -> badRequest

route (Just staticDir) (Message (GET, '/' : 'f' : 'i' : 'l' : 'e' : 's' : '/' : filepath, _) _ _) = 
    fileIO `catch` (\(_ :: IOException) -> return err404)
    where 
        fileIO = ((liftM respond) . BC.readFile $ staticDir <> filepath)
        respond content = B.concat [
            "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: ",
            contentLength,
            "\r\n\r\n",
            content]
            where contentLength = BC.pack . show . B.length $ content

route _ _ = return err404

-- Simple lookup in a list of headers since we aren't allowed to use a Map?
getHeader :: [Header] -> BC.ByteString -> Maybe BC.ByteString
getHeader headers key = (fmap snd) (find ((== key) . fst) headers)

badRequest :: BC.ByteString
badRequest = BC.pack "HTTP/1.1 400 Bad Request\r\n\r\n"

err404 :: BC.ByteString
err404 = BC.pack "HTTP/1.1 404 Not Found\r\n\r\n"

err401 :: BC.ByteString
err401 = BC.pack "HTTP/1.1 401 Not Found\r\n\r\n"
