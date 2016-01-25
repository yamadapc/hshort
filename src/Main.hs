{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- The main entry point for the application.
module Main where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative           ((<$>))
#endif
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Char8         as BC (pack)
import           Data.Maybe                    (fromMaybe, isJust)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T (pack, unpack)
import qualified Database.Redis                as Redis
import           Network.Wai.Middleware.Static
import           System.Environment            (getEnv, lookupEnv)
import           Web.Scotty.Hastache           (scottyH', setTemplatesDir)
import           Web.Scotty.Trans              (file, get, middleware)

import qualified Controllers.Urls              as UrlsCtrl (loadRoutes)

main :: IO ()
main = do
    port <- read <$> (fromMaybe "3000" <$> lookupEnv "PORT")
    host <- T.pack <$>
        (fromMaybe ("http://localhost:" <> show port) <$> lookupEnv "HOST")


    let hostname = host <> ":" <> T.pack (show port)
    liftIO $
        putStrLn $ "Starting server at " ++ T.unpack hostname

    !info <- getRedisConnectInfo
    !conn <- Redis.connect info

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
        UrlsCtrl.loadRoutes host conn

getRedisConnectInfo :: IO Redis.ConnectInfo
getRedisConnectInfo = do
    !redisHost  <- "REDIS_HOST" `fromEnvWithDefault` "127.0.0.1"
    !redisPort  <- read <$> "REDIS_PORT" `fromEnvWithDefault` "6379" :: IO Int
    !mRedisAuthString <- lookupEnv "REDIS_AUTH"
    let !mRedisAuth = BC.pack <$> mRedisAuthString

    return $ Redis.defaultConnectInfo { Redis.connectHost = redisHost
                                      , Redis.connectPort = Redis.PortNumber $
                                            fromIntegral redisPort
                                      , Redis.connectAuth = mRedisAuth
                                      }
  where
    fromEnvWithDefault e d = fromMaybe d <$> lookupEnv e
