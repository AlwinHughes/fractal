{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_frac (
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

bindir     = "/home/alwin/.cabal/bin"
libdir     = "/home/alwin/.cabal/lib/x86_64-linux-ghc-8.2.1/frac-0.1.0.0-HiSAn5pI6MvEawvTJB0SMQ-frac"
dynlibdir  = "/home/alwin/.cabal/lib/x86_64-linux-ghc-8.2.1"
datadir    = "/home/alwin/.cabal/share/x86_64-linux-ghc-8.2.1/frac-0.1.0.0"
libexecdir = "/home/alwin/.cabal/libexec/x86_64-linux-ghc-8.2.1/frac-0.1.0.0"
sysconfdir = "/home/alwin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "frac_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "frac_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "frac_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "frac_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "frac_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "frac_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
