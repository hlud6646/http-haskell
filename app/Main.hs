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

    let host = "127.0.0.1"
        port = "4221"
    
    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port
    
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    
    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5
    
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

        (req, _, _) <- parseMsg <$> recv clientSocket 1024
        sendAll clientSocket $ case parseReq req of
            -- TODO: Is this an appropriate response?
            Nothing -> "HTTP/1.1 500 Internal Server Error\r\n\r\n"
            Just (method, target, _) -> route method (BC.unpack target)

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

-- TODO: Use Method datatype not bytestring.
route :: BC.ByteString -> String -> BC.ByteString
route "GET" "/" = "HTTP/1.1 200 OK\r\n\r\n"
route "GET" ('/' : 'e' : 'c' : 'h' : 'o' : '/' : text) = mkEchoResponse text
route _ _  = "HTTP/1.1 404 Not Found\r\n\r\n"

mkEchoResponse :: String -> BC.ByteString
mkEchoResponse text = BC.pack $ "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> contentLength <> "\r\n\r\n" <> text
    where contentLength = show . length $ text
