module Paths_calc (
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
version = Version {versionBranch = [1,0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/srv/www/upopple.com/slack/calculator/.cabal-sandbox/bin"
libdir     = "/srv/www/upopple.com/slack/calculator/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.2/calc-1.0.0.0"
datadir    = "/srv/www/upopple.com/slack/calculator/.cabal-sandbox/share/x86_64-linux-ghc-7.8.2/calc-1.0.0.0"
libexecdir = "/srv/www/upopple.com/slack/calculator/.cabal-sandbox/libexec"
sysconfdir = "/srv/www/upopple.com/slack/calculator/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "calc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "calc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "calc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
