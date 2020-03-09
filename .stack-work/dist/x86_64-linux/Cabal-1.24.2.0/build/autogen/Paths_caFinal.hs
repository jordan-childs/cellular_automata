{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_caFinal (
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

bindir     = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/bin"
libdir     = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/lib/x86_64-linux-ghc-8.0.2/caFinal-0.1.0.0"
dynlibdir  = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/share/x86_64-linux-ghc-8.0.2/caFinal-0.1.0.0"
libexecdir = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/libexec"
sysconfdir = "/home/jordan/Documents/school/07-winter20/functional/ca-final/.stack-work/install/x86_64-linux/dac214c5b6b16309e4f1c341b29d7a105ddaa16853f688411a92181dba0ff5c3/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "caFinal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "caFinal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "caFinal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "caFinal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "caFinal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "caFinal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
