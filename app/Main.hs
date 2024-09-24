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
            let req = parse msgString
                encs = case req of 
                    Nothing -> []
                    Just r -> encodings r
            res <- case req of
                Nothing -> return err400' 
                Just r -> route' directory r
            if (Gzip `elem` encs)
            then sendAll client (pack Gzip res)
            else sendAll client (pack Id res)

    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        _ <- forkFinally (respond clientSocket) (\_ -> close clientSocket)
        return ()


data Method = GET | POST | PATCH | DELETE
    deriving (Eq, Read)
type Target = String -- Not ByteString since we will pattern match on this.
type Version = BC.ByteString
type StatusCode = BC.ByteString
type StatusText = BC.ByteString
type Header = (BC.ByteString, BC.ByteString)
type Body = BC.ByteString
data Request = Request (Method, Target, Version) [Header] Body
data Response = Response (Version, StatusCode, StatusText) [Header] Body
data Encoding = Gzip | MadeUpEncoding | Id
    deriving (Read, Eq)


-- ------------------------------------------------------------------------------------------------
-- Reading+Showing Requests+Responses.

-- Unfortunate that we are not permitted to use a parsing library here.
parse :: BC.ByteString -> Maybe Request
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
    return $ Request (method, target, version) headers body
    
pack :: Encoding -> Response -> BC.ByteString
pack Gzip (Response statusLine headers body) =
    packStatusLine statusLine <> "\r\n" <>
    packHeaders (("Content-Encoding", "gzip") : headers) <> "\r\n" <>
    gzipBody where
        gzipBody = body
        
pack _ (Response statusLine  headers body) = 
    packStatusLine statusLine <> "\r\n" <>
    packHeaders headers <> "\r\n" <>
    body

packStatusLine :: (Version, StatusCode, StatusText) -> BC.ByteString
packStatusLine (version, statusCode, statusText) = BC.unwords [version, statusCode, statusText]

packHeaders :: [Header] -> BC.ByteString
packHeaders = BC.concat . (map (\(k, v) -> k <> ": " <> v <> "\r\n"))

encodings :: Request -> [Encoding]
encodings (Request _ headers _) = case getHeader headers "Accept-Encoding" of 
    Nothing -> []
    Just encs -> catMaybes (map readEnc $ (words . (filter (/= ',')) . BC.unpack) encs) where
        readEnc "gzip" = Just Gzip
        readEnc "madeUpEncoding" = Just MadeUpEncoding
        readEnc _ = Nothing 
-- ------------------------------------------------------------------------------------------------


-- ------------------------------------------------------------------------------------------------
-- Examples of server functionality.
route' :: Maybe FilePath -> Request -> IO Response
route' _ (Request (GET, "/", _) _ _) = return $ Response ok [] ""

route' _ (Request (GET, '/' : 'e' : 'c' : 'h' : 'o' : '/' : text, _) _ _) = do
    return $ Response ok headers (BC.pack text) where
        headers = [ 
            ("Content-Type", "text/plain"),
            ("Content-Length", BC.pack . show . length $ text)]

route' _ (Request (GET, "/user-agent", _) reqHeaders _) = 
    return $ case getHeader reqHeaders "User-Agent" of
        Just userAgent -> Response ok resHeaders userAgent where
            resHeaders = [ 
                ("Content-Type", "text/plain"),
                ("Content-Length", BC.pack . show . BC.length $ userAgent)]
        Nothing -> err400'

route' (Just staticDir) (Request (GET, '/' : 'f' : 'i' : 'l' : 'e' : 's' : '/' : filepath, _) _ _) = 
    fileIO `catch` (\(_ :: IOException) -> return err404')
    where 
        fileIO = ((liftM respond) . BC.readFile $ staticDir <> filepath)
        respond content = Response ok headers content where
            headers = [
                ("Content-Type", "application/octet-stream"),
                ("Content-Length", BC.pack . show . B.length $ content)]

route' (Just staticDir) (Request (POST, '/' : 'f' : 'i' : 'l' : 'e' : 's' : '/' : filepath, _) _ body) = do
    _ <- BC.writeFile (staticDir <> filepath) (BC.strip body)
    return $ Response ("HTTP/1.1", "201", "Created") [] ""

route' _ _ = return $ err404'
-- ------------------------------------------------------------------------------------------------



-- ------------------------------------------------------------------------------------------------
-- Utils

-- Simple lookup in a list of headers since we aren't allowed to use a Map?
getHeader :: [Header] -> BC.ByteString -> Maybe BC.ByteString
getHeader headers key = (fmap snd) (find ((== key) . fst) headers)

err400' :: Response
err400' = Response ("HTTP/1.1", "404", "Bad Request") [] ""

err404' :: Response
err404' = Response ("HTTP/1.1", "404", "Not Found") [] ""

ok :: (Version, StatusCode, StatusText)
ok = ("HTTP/1.1", "200", "OK")
