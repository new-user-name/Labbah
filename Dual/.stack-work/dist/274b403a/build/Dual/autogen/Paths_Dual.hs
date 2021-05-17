{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Dual (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\bin"
libdir     = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\lib\\x86_64-windows-ghc-8.10.4\\Dual-0.1.0.0-5nsCJrouQh1FM8QoFiw6TE-Dual"
dynlibdir  = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\share\\x86_64-windows-ghc-8.10.4\\Dual-0.1.0.0"
libexecdir = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\libexec\\x86_64-windows-ghc-8.10.4\\Dual-0.1.0.0"
sysconfdir = "D:\\Haskell\\Dual\\.stack-work\\install\\b61ca32a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Dual_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Dual_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Dual_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Dual_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Dual_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Dual_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
