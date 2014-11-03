-- |
-- Module:      Main.Util
-- Copyright:   (c) 2014 Tomislav Viljetić
-- License:     BSD3
-- Maintainer:  Tomislav Viljetić <tomislav@viljetic.de>
--
-- Grab bag of utilities used by "Main".  This module is used to keep
-- "Main" as application-focused as possible.
--

{-# LANGUAGE OverloadedStrings #-}


module Main.Util
    (
      -- * Data.List utilities
      splitLast,
      -- * Network.Wai utilities
      pathInfoString,
      handleMethod,
      okJSON,
      noContent,
      badRequest,
      badRequest',
      forbidden,
      notFound,
      notAllowed,
      conflict,
      conflict',
      internalServerError,
      internalServerError',
    ) where

import Data.Aeson (encode, ToJSON)
import Data.Monoid
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types
import Network.Wai
import Safe



splitLast :: [a] -> ([a], a)
splitLast xs =
    (init xs, last xs)


-- | Like 'pathInfo', but returns 'String's instead.
pathInfoString :: Request -> [String]
pathInfoString = map T.unpack . pathInfo


-- | Route a request based on it's method.
-- If no application is associated with the request's method,
-- then 'notAllowed' is used.
handleMethod :: [(Method, Application)] -> Application
handleMethod apps req respond =
    app req respond
  where
    app = lookupJustDef (notAllowed allow) (requestMethod req) apps
    allow = map fst apps


okJSON :: ToJSON a => a -> Application
okJSON = respondJSON [] ok200


noContent :: Application
noContent = respondEmpty [] noContent204


badRequest :: Application
badRequest = respondEmpty [] badRequest400


badRequest' :: LT.Text -> Application
badRequest' = respondText [] badRequest400


forbidden :: Application
forbidden = respondEmpty [] forbidden403


notFound :: Application
notFound = respondEmpty [] notFound404


notAllowed :: [Method] -> Application
notAllowed allow =
    respondEmpty [(hAllow, BS8.intercalate ", " allow)] methodNotAllowed405


conflict :: Application
conflict =
    respondEmpty [] conflict409


conflict' :: BS.ByteString -> Application
conflict' msg =
    respondEmpty [] status
  where
    status = mkStatus 409 $ "Conflict (" <> msg <> ")"


internalServerError :: Application
internalServerError =
    respondEmpty [] internalServerError500


internalServerError' :: BS.ByteString -> Application
internalServerError' msg =
    respondEmpty [] status
  where
    status = mkStatus 500 $ "Internal Server Error (" <> msg <> ")"



-- XXX currently it's not always possible to send a truly empty response.
-- because 'Network.Wai.Handler.Warp.Response.hasBody' only discriminates
-- by status code and request method.  This means that empty responses may
-- contain superfluous headers like @Transfer-Encoding@.  This behavior
-- should not cause any problems, though.
respondEmpty :: ResponseHeaders -> Status -> Application
respondEmpty extraHeaders status =
    respondLBS status extraHeaders ""


respondJSON :: ToJSON a => ResponseHeaders -> Status -> a -> Application
respondJSON extraHeaders status =
    respondLBS status headers . encode
  where
    headers = (hContentType, "application/json") : extraHeaders


respondText :: ResponseHeaders -> Status -> LT.Text -> Application
respondText extraHeaders status =
    respondLBS status headers . LT.encodeUtf8
  where
    headers = (hContentType, "text/plain; charset=utf-8") : extraHeaders



respondLBS :: Status -> ResponseHeaders -> LBS.ByteString -> Application
respondLBS status headers bs _req respond =
    respond $ responseLBS status headers bs



-- | HTTP Header names, missing from 'Network.HTTP.Types.Header'.

hAllow :: HeaderName
hAllow = "Allow"
