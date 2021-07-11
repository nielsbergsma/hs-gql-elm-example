{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}

module Main where

import Network.Wai (Application, Request, Response, requestMethod, pathInfo, responseLBS, strictRequestBody)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (methodPost, status200)
import qualified Resolvers as R
import qualified Projections as P

-- settings
apiPort = 3000
connectionString = "postgres://postgres:secret@localhost/experiments"
staticFilesDirectory = "public"

-- main
main :: IO ()
main = do
  db <- P.connect connectionString
  _  <- putStrLn ("API is listening on :" ++ show apiPort)

  Warp.run apiPort $ route db

-- http
route :: P.Connection -> Application
route db request send = case (requestMethod request, pathInfo request) of
  (methodPost, ["api"]) -> send =<< gql db request
  _                     -> staticFile request send

gql :: P.Connection -> Request -> IO Response
gql db request = do
  body     <- strictRequestBody request
  response <- R.resolve context body
  return $ responseLBS status200 [] response

  where 
    context = R.newContext db

staticFile :: Application
staticFile = staticApp (defaultFileServerSettings staticFilesDirectory)