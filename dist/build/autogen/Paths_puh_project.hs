module Paths_puh_project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/herc/.cabal/bin"
libdir     = "/home/herc/.cabal/lib/puh-project-0.1.0.0/ghc-7.6.3"
datadir    = "/home/herc/.cabal/share/puh-project-0.1.0.0"
libexecdir = "/home/herc/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "puh_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "puh_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "puh_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "puh_project_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
