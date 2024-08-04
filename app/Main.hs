{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    -- BC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"
    
    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port
    
    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    
    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5
    
    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

        (req, _, _) <- parseMsg <$> recv clientSocket 1024
        sendAll clientSocket $ case parseReq req of
            Nothing -> "?"
            Just (_, "/", _) -> "HTTP/1.1 200 OK\r\n\r\n"
            _ -> "HTTP/1.1 404 Not Found\r\n\r\n"

        close clientSocket

parseMsg :: BC.ByteString -> (BC.ByteString, BC.ByteString, BC.ByteString)
parseMsg msg = (req, headers, body)
    where (req, rest) = B.breakSubstring "\r\n" msg
          (headers, body) = B.breakSubstring "\r\n\r\n" rest

parseReq :: B.ByteString -> Maybe (B.ByteString, B.ByteString, B.ByteString)
parseReq req = case  BC.split ' ' req of 
    [method, target, version] ->  Just (method, target, version)
    _ -> Nothing


data Method = GET | POST | PATCH | DELETE
    deriving (Eq, Show, Read)

-- Not given megaparsec, which I guess is too much for this.
