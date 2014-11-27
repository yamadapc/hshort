{-# LANGUAGE OverloadedStrings #-}
-- |
-- The main entry point for the application.
module Main where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack)
import Data.Word (Word16)
import qualified Database.Redis as Redis
import Network.Socket (PortNumber(PortNum))
import Network.Wai.Middleware.Static
import System.Environment (getEnv, lookupEnv)
import Web.Scotty.Trans (get, file, middleware)
import Web.Scotty.Hastache (ActionH', scottyH', setTemplatesDir)

import qualified Controllers.Urls as UrlsCtrl (loadRoutes)

main :: IO ()
main = do
    host <- fmap T.pack (getEnv "HOST")
    port <- fmap read (getEnv "PORT")


    let hostname = host <> ":" <> T.pack (show port)
    liftIO $
        putStrLn $ "Starting server at " ++ T.unpack hostname

    info <- getRedisConnectInfo
    conn <- Redis.connect info

    let Redis.PortNumber pn = Redis.connectPort info
      in liftIO $
          putStrLn $
              "Redis connection expected at " ++ Redis.connectHost info ++
              ":" ++ show pn

    liftIO . putStrLn $ if isJust (Redis.connectAuth info)
        then "Redis Authentication configured"
        else "Redis Authentication not configured"

    scottyH' port $ do
        setTemplatesDir "templates"

        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ file "static/index.html"
        UrlsCtrl.loadRoutes hostname conn

getRedisConnectInfo :: IO Redis.ConnectInfo
getRedisConnectInfo = do
    redisHost  <- "REDIS_HOST" `fromEnvWithDefault` "127.0.0.1"
    redisPort  <- read <$> "REDIS_PORT" `fromEnvWithDefault` "6379" :: IO Int
    mRedisAuth <- fmap read <$> lookupEnv "REDIS_AUTH"
    return $ Redis.defaultConnectInfo { Redis.connectHost = redisHost
                                      , Redis.connectPort = Redis.PortNumber $
                                            fromIntegral redisPort
                                      , Redis.connectAuth = mRedisAuth
                                      }
  where fromEnvWithDefault e d = fromMaybe d <$> lookupEnv e
