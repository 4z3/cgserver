-- |
-- Module:      Main.Config
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--


module Main.Config (Config(..), defaultConfig) where

import Network.Wai.Handler.Warp (Port)


data Config = Config
    { cgroupRoot :: FilePath
    , httpPort :: Port
    }


-- |
--
-- > cgroupRoot = "/sys/fs/cgroup"
-- > httpPort = 8001
--
defaultConfig :: Config
defaultConfig = Config
    { cgroupRoot = "/sys/fs/cgroup"
    , httpPort = 8001
    }
