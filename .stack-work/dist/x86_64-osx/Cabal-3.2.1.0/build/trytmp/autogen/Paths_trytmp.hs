{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_trytmp (
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

bindir     = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/bin"
libdir     = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/lib/x86_64-osx-ghc-8.10.4/trytmp-0.1.0.0-KSb9243T9gMBvnR47nOi0O-trytmp"
dynlibdir  = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/share/x86_64-osx-ghc-8.10.4/trytmp-0.1.0.0"
libexecdir = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/libexec/x86_64-osx-ghc-8.10.4/trytmp-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/bitbucket/stackproject/trytmp/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trytmp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trytmp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "trytmp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "trytmp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trytmp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trytmp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
