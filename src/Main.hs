{-# LANGUAGE OverloadedStrings #-}
-- |
-- The main entry point for the application.
module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString as ByteStringS
import qualified Data.ByteString.Lazy as ByteStringL (fromStrict, toStrict)
import Data.Char (ord)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Data.Word (Word8)
import qualified Database.Redis as Redis
import Network.HTTP.Types
import Network.Wai.Middleware.Static
import System.Environment (getEnv)
import System.Random
import Web.Scotty

main :: IO ()
main = do
    port <- fmap read (getEnv "PORT")
    conn <- Redis.connect Redis.defaultConnectInfo

    scotty port $ do
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" handleHome
        get "/:url_hash" (handleUrlRedirect conn)
        post "/urls" (handleUrlCreate conn)
        get "/urls/:url_hash" handleUrlView

handleHome :: ActionM ()
handleHome = file "static/index.html"

handleUrlRedirect :: Redis.Connection -> ActionM ()
handleUrlRedirect conn = do
    k <- param "url_hash"
    mr <- liftIO $ getRedirectUrl conn k
    case mr of
        Just r -> redirect r
        Nothing -> status status404 >> text "Invalid URL"

handleUrlView :: ActionM ()
handleUrlView = undefined

handleUrlCreate :: Redis.Connection -> ActionM ()
handleUrlCreate conn = do
    url <- fmap ByteStringL.toStrict body
    if validateUrl url then do
                           url' <- liftIO $ createUrlFromString conn url
                           status status201
                           text url'
                       else status status400 >> text "Invalid URL"

validateUrl :: ByteStringS.ByteString -> Bool
validateUrl _ = True

getRedirectUrl :: Redis.Connection -> TextL.Text -> IO (Maybe TextL.Text)
getRedirectUrl conn k = do
    rd <- Redis.runRedis conn $ Redis.get $ ByteStringL.toStrict $
                                            TextL.encodeUtf8 k

    case rd of
        Left _ -> fail "Errored communicating with Redis"
        Right (Just s) -> return $ Just $ TextL.decodeUtf8 $
                                          ByteStringL.fromStrict s
        Right Nothing -> return Nothing

createUrlFromString :: Redis.Connection ->
                       ByteStringS.ByteString ->
                       IO TextL.Text
createUrlFromString conn url = do
    k <- getRandomKey
    i <- Redis.runRedis conn $ Redis.setnx k url

    case i of
        Left _ -> fail "Errored communicating with Redis"
        Right True  -> return $ "localhost:3000/" `TextL.append`
                                TextL.decodeUtf8 (ByteStringL.fromStrict k)
        Right False -> createUrlFromString conn url

getRandomKey :: IO ByteStringS.ByteString
getRandomKey = do
    s <- loop 5 []
    return $ ByteStringS.pack s
  where loop :: Int -> [Word8] -> IO [Word8]
        loop 0 acc = return acc
        loop n acc = do
          c <- randomRIO (toWord8 'a', toWord8 'z')
          loop (n - 1) (c:acc)

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord
