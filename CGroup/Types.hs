-- |
-- Module:      CGroup.Types
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--

module CGroup.Types
    ( CGroup
    , cgroup
    , cgroupPath
    , ProcessID
    ) where

import Data.Monoid
import qualified System.FilePath as FP


-- | A 'CGroup' is defined by a mount point and a cgroup name.
--
-- The mount point specifies where the cgroup hierarchy is mounted.
-- The cgroup name is a 'FilePath' relative to the mount point.
data CGroup = CGroup { mountPoint, cgroupName :: FilePath }
  deriving Show


-- | @'cgroup' mountPoint cgroupName@ is a smart constructor for 'CGroup'.
--
-- It will return 'Nothing' if @cgroupName@ could point outside
-- @mountPoint@ in order to prevent directory traversal attacks.
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


-- | @'cgroupPath' g@ returns the absolute 'FilePath' of cgroup @g@.
cgroupPath :: CGroup -> FilePath
cgroupPath CGroup { mountPoint = mp, cgroupName = cgn } =
    mp <> cgn


-- | A 'ProcessID' defines a task / process.
type ProcessID = Int
