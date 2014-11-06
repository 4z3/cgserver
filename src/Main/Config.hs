-- |
-- Module:      Main.Config
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--


module Main.Config (Config(..), defaultConfig) where

import Control.Applicative
import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Monoid
import Network.Wai.Handler.Warp (Port)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readEither)


data Config = Config
    { cgroupRoot :: FilePath
    -- ^ Mount point of the cgroup root.
    , httpPort :: Port
    -- ^ TCP port number for cgserver to bind to.
    , flushLog :: Bool
    -- ^ Whether to flush the logging buffer after each request.
    }
  deriving Show


-- |
-- The default configuration gets read from the environment variables
-- @cgroupRoot@ and @httpPort@.
--
-- If either doesn't exist, then their respective default value gets used:
--
-- > cgroupRoot = "/sys/fs/cgroup"
-- > httpPort = 8001
-- > flushLog = True
--
defaultConfig :: IO Config
defaultConfig =
    Config
        <$> getEnv' Right  "/sys/fs/cgroup" "cgroupRoot"
        <*> getEnv' readEither 8001 "httpPort"
        <*> getEnv' readBool True "flushLog"


-- | Takes a parse function, a default value, and a variable name.
getEnv' :: (String -> Either String a) -> a -> String -> IO a
getEnv' pf def name =
    either (const def) parse <$>
        tryJust (guard . isDoesNotExistError) (getEnv name)
  where
    parse rawValue =
        case pf rawValue of
            Left err ->
                error $ "Main.Config.getEnv' " <> show name <> ": " <> err
            Right value ->
                value


-- | Read a JSON-style boolean ("true", "false").
readBool :: String -> Either String Bool
readBool x = case x of
    "true" -> Right True
    "false" -> Right False
    _ -> Left $ "not a bool: " <> x
