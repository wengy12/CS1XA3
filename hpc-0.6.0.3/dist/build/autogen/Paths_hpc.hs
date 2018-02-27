module Paths_hpc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,6,0,3] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/u50/wengy12/.cabal/bin"
libdir     = "/u50/wengy12/.cabal/lib/x86_64-linux-ghc-7.10.3/hpc-0.6.0.3-9sbUIPzVKfRLFjlm8xT1BU"
datadir    = "/u50/wengy12/.cabal/share/x86_64-linux-ghc-7.10.3/hpc-0.6.0.3"
libexecdir = "/u50/wengy12/.cabal/libexec"
sysconfdir = "/u50/wengy12/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hpc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hpc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hpc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hpc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hpc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
