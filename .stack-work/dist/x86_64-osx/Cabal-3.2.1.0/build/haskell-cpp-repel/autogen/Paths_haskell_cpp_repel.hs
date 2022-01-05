{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_cpp_repel (
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

bindir     = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/bin"
libdir     = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/lib/x86_64-osx-ghc-8.10.4/haskell-cpp-repel-0.1.0.0-K2tomsV37mv4mhynrLc1ba-haskell-cpp-repel"
dynlibdir  = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/share/x86_64-osx-ghc-8.10.4/haskell-cpp-repel-0.1.0.0"
libexecdir = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/libexec/x86_64-osx-ghc-8.10.4/haskell-cpp-repel-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/github/haskell-cpp-repel/.stack-work/install/x86_64-osx/f4a8892df9ec3dccaeadb09f6669366126267da4d08e7f5a0ec41220ce398cba/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_cpp_repel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_cpp_repel_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_cpp_repel_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_cpp_repel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_cpp_repel_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_cpp_repel_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
