{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hs_http_server_clone (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/erg/.cabal/bin"
libdir     = "/home/erg/.cabal/lib/x86_64-linux-ghc-9.4.7/hs-http-server-clone-0.1.0.0-inplace-hs-http-server-clone-exe"
dynlibdir  = "/home/erg/.cabal/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/erg/.cabal/share/x86_64-linux-ghc-9.4.7/hs-http-server-clone-0.1.0.0"
libexecdir = "/home/erg/.cabal/libexec/x86_64-linux-ghc-9.4.7/hs-http-server-clone-0.1.0.0"
sysconfdir = "/home/erg/.cabal/etc"

getBinDir     = catchIO (getEnv "hs_http_server_clone_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hs_http_server_clone_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hs_http_server_clone_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hs_http_server_clone_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hs_http_server_clone_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hs_http_server_clone_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
