-- |
-- Module:      CGroup.Types
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--

module CGroup.Types
    (
    -- * CGroup
      CGroup
    , cgroup
    , cgroupPath
    -- * Other types
    , ProcessID
    ) where

import Data.Monoid
import qualified System.FilePath as FP


-- | A 'CGroup' is defined by two 'FilePath's, a mount point and a cgroup
-- name.  The mount point specifies where the cgroup hierarchy is mounted.
-- The cgroup name is a directory, relative to the mount point.
data CGroup = CGroup { mountPoint, cgroupName :: FilePath }
  deriving Show


-- | Smart constructor. Takes a mount point and a cgroup name.
-- It will return 'Nothing' if the cgroup could point outside the mount point,
-- i.e. if the cgroup name is an absolute path, or contains @".."@.
cgroup :: FilePath -> FilePath -> Maybe CGroup
cgroup mp0 cgn0
    | ".." `elem` parts = Nothing
    | FP.isAbsolute cgn = Nothing
    | otherwise = Just CGroup { mountPoint = mp, cgroupName = cgn }
  where
    mp = normaliseMountPoint mp0
    cgn = normaliseCGroupName cgn0
    parts = FP.splitDirectories cgn
    normaliseMountPoint = FP.addTrailingPathSeparator . FP.normalise
    normaliseCGroupName = FP.dropTrailingPathSeparator . FP.normalise


-- | Path of a cgroup's tasks file.
cgroupPath :: CGroup -> FilePath
cgroupPath CGroup { mountPoint = mp, cgroupName = cgn } =
    mp <> cgn


type ProcessID = Int
