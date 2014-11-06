-- |
-- Module:      Main
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--
-- An 'Application' that provides a HTTP API to manage cgroups.
--
-- The API documentation isn't formalized, but examples can be found in the
-- description of the resources handlers ('putCGroupH', 'postTasksH', and
-- 'getTasksH').  The examples only contain HTTP headers that are relevant to
-- the handlers.  A real request may require further headers (such as @Host@)
-- to be effective.  In addition the HTTP version is omitted in both, the
-- request line and the status line.
--

{-# LANGUAGE OverloadedStrings #-}


module Main
    (
      -- * Run the application
      main,
      start,

      -- * Resource
      Resource(..),
      requestResource,
      resourceHandler,

      -- ** Resource Handler
      cgroupH,
      tasksH,

      -- *** CGroup Handler
      putCGroupH,

      -- *** Task File Handler
      postTasksH,
      getTasksH,

    ) where

import CGroup
import Control.Applicative
import Control.Exception
import Data.Attoparsec.ByteString.Char8 (decimal, endOfInput, parseOnly)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (isInfixOf)
import Data.Monoid
import Main.Config
import Main.Util
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.FilePath
import System.IO.Error
import System.IO (hFlush, stdout)


-- | The sum of all resources known by the application.
-- This type is used to route a 'Request' to a resource handler
-- 'Application'.
data Resource
    = CGroupR CGroup
        -- ^ A cgroup.
    | TasksR CGroup
        -- ^ A cgroup's tasks file.


-- | Run the application with the default configuration.
-- This calls 'start' with 'defaultConfig'.
main :: IO ()
main =
    start =<< defaultConfig


-- | Run the application with the given configuration.
start :: Config -> IO ()
start c = do
    putStrLn $ "run cgserver with " <> show c
    hFlush stdout
    run (httpPort c)
        $ logger
        $ \req ->
            resourceHandler (requestResource c req) req
  where
    logger =
        if flushLog c
            then flush stdout . logStdout
            else logStdout

    -- This middleware flushes the given handle after each request.
    flush h app req respond = app req $ \res -> do
            hFlush h
            respond res


-- | Determine which request is requested.
requestResource :: Config -> Request -> Maybe Resource
-- TODO Config should contain a list of all filenames that cannot be used as
-- cgroup name.  This implies new error modes
requestResource c req =
    if length parts > 1
        then case splitLast parts of
                (initparts, "tasks") ->
                    TasksR <$> toCGroup initparts
                _ ->
                    CGroupR <$> toCGroup parts
        else Nothing
  where
    parts = pathInfoString req
    toCGroup (phead:ptail) = cgroup (toMountPoint phead) (joinPath ptail)
    toCGroup _ = error "App.route.toCGroup: empty list"
    toMountPoint = (cgroupRoot c </>)


-- | Return the resource handler for a specific resource.
resourceHandler :: Maybe Resource -> Application
resourceHandler r = case r of
    Just (CGroupR g) -> cgroupH g
    Just (TasksR g) -> tasksH g
    Nothing -> notFound


cgroupH :: CGroup -> Application
cgroupH g =
    handleMethod
        [ ("PUT", putCGroupH g)
        ]

tasksH :: CGroup -> Application
tasksH g =
    handleMethod
        [ ("GET", getTasksH g)
        , ("POST", postTasksH g)
        ]


-- | Create a new cgroup.
--
-- __Example:__
-- (Create a new cgroup @users\/alice@ in the hierarchy @cpu@.)
--
-- > PUT /cpu/users/alice HTTP/1.1
--
--
-- If the request was successful, then the server will respond with:
--
-- > HTTP/1.1 204 No Content
--
-- The request may fail with:
--
-- * @403 Forbidden@
--   The servers has no permission to create the cgroup.
--
-- * @404 Not Found@
--   Either the hierarchy @cpu@ or, when creating a subcgroup,
--   the cgroup @users@ does not exist.
--
-- * @409 Conflict@
--   The cgroup already exists.
--
-- * @500 Internal Server Error@
--   Calling 'System.Directory.createDirectory' failed for any other reason.
--
putCGroupH :: CGroup -> Application
putCGroupH g req respond = do
    x <- try $ createCGroup g
    either failure success x req respond
  where
    success () = noContent
    failure e
        | isPermissionError e     = forbidden
        | isAlreadyExistsError e  = conflict
        | isDoesNotExistError e   = notFound
        | otherwise               = internalServerError' $ BS8.pack $ show e


-- | Place a process into a cgroup.
--
-- __Example:__
-- (Move process @1337@ to cgroup @users\/alice@ of the hierarchy @cpu@.)
--
-- > POST /cpu/users/alice/tasks HTTP/1.1
-- >
-- > 1337
--
--
-- If the request was successful, then the server will respond with:
--
-- > HTTP/1.1 204 No Content
--
--
-- The request may fail with:
--
-- * @400 Bad Request@
--   The request body does not contain a decimal representation of a PID.
--
-- * @403 Forbidden@
--   The servers has no permission to open the tasks file for writing.
--
-- * @404 Not Found@
--   The cgroup doesn't exist.
--
-- * @409 Conflict (Cannot Move Process)@
--   The servers has no permission to move the process @1337@ to the cgroup.
--
-- * @409 Conflict (No Such Process)@
--   The process @1337@ doesn't exist.
--
-- * @500 Internal Server Error@
--   Calling 'System.IO.writeFile' failed for any other reason.
--
postTasksH :: CGroup -> Application
postTasksH g req respond = do
    b <- LBS.toStrict <$> strictRequestBody req
    case parseOnly (decimal <* endOfInput) b of
        Left _ ->
            badRequest req respond
        Right pid -> do
            x <- try $ classifyTask pid g
            either failure success x req respond
  where
    success () = noContent

    -- XXX string-typed exception handler
    --
    -- We're analyzing the error string to tell if there's a problem with
    --
    -- * the task (Conflict; Cannot Move Process, No Such Process)
    -- * the cgroup (Forbidden, NotFound)
    --
    -- TODO replace stringly-typed exceptions with real type.
    -- In 'classifyTask', replace 'writeFile' by explicit calls to 'openFile'
    -- and 'hClose' in order tell apart the error cases.
    failure e
        | isPermissionError e =
            if isOpenFileError e
                then forbidden
                else conflict' "Cannot Move Process"
        | isDoesNotExistError e =
            if isOpenFileError e
                then notFound
                else conflict' "No Such Process"
        | otherwise =
            internalServerError' $ BS8.pack $ show e
      where
        isOpenFileError :: IOError -> Bool
        isOpenFileError =
            isInfixOf "openFile" . show


-- | List the tasks (PIDs) for a given cgroup.
--
--
-- __Example:__
-- (Retrieve all tasks of cgroup @users\/alice@ of the hierarchy @cpu@.)
--
-- > GET /cpu/alice/tasks HTTP/1.1
--
--
-- If the request was successful, then the server will respond with:
--
-- > HTTP/1.1 200 OK
-- > Content-Type: application/json
-- >
-- > [1337]
--
--
-- The request may fail with:
--
-- * @403 Forbidden@
--   If the server has no permission to read to the tasks file:
--
-- * @404 Not Found@
--   If the cgroup doesn't exist:
--
-- * @500 Internal Server Error@
--   Calling 'System.IO.Streams.File.withFileAsInput' failed for any other
--   reason.
--
getTasksH :: CGroup -> Application
getTasksH g req respond = do
    x <- try $ listTasks g
    either failure success x req respond
  where
    success = okJSON
    failure e
        | isPermissionError e   = forbidden
        | isDoesNotExistError e = notFound
        | otherwise             = internalServerError' $ BS8.pack $ show e
