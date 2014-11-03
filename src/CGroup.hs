-- |
-- Module:      CGroup
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--
-- Basic cgroup virtual filesystem operations.
--

module CGroup
    ( module CGroup.Types
    , createCGroup
    , classifyTask
    , listTasks
    ) where

import CGroup.Types
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Streams.Attoparsec (parseFromStream)
import System.IO.Streams.File (withFileAsInput)


-- | Create a new cgroup.
createCGroup :: CGroup -> IO ()
createCGroup =
    createDirectory . cgroupPath


-- | Places a task into a cgroup.
classifyTask :: ProcessID -> CGroup -> IO ()
classifyTask pid g =
    writeFile (tasksFile g) (show pid)


-- | Retrieve the tasks of a cgroup.
listTasks :: CGroup -> IO (Set ProcessID)
listTasks g =
    withFileAsInput (tasksFile g) $ parseFromStream tasksParser


tasksFile :: CGroup -> FilePath
tasksFile =
    (</> "tasks") . cgroupPath


tasksParser :: Parser (Set ProcessID)
tasksParser =
    Set.fromList <$> many' (decimal <* endOfLine) <* endOfInput <?> "tasks"
