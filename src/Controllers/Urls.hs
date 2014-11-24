{-# LANGUAGE OverloadedStrings #-}
module Controllers.Urls
    ( urls
    , handleUrlCreate
    , handleUrlView
    )
  where

import Control.Monad.IO.Class (liftIO)
import qualified Database.Redis as Redis
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Char (ord)
import Data.Monoid (mappend)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word (Word8)
import Network.HTTP.Types (status201, status400)
import System.Random
import Web.Scotty (ActionM, ScottyM, body, status, text)

urls :: Redis.Connection -> ScottyM ()
urls = undefined

handleUrlCreate :: Redis.Connection -> ActionM ()
handleUrlCreate conn = do
    url <- fmap BL.toStrict body
    if validateUrl url
        then do
            url' <- liftIO $ createUrlFromString conn url
            status status201
            text url'
        else status status400 >> text "Invalid URL"

handleUrlView :: Redis.Connection -> ActionM ()
handleUrlView = undefined

validateUrl :: B.ByteString -> Bool
validateUrl _ = True

createUrlFromString :: Redis.Connection -> B.ByteString -> IO TL.Text
createUrlFromString conn url = do
    k <- getRandomKey
    i <- Redis.runRedis conn $ Redis.setnx k url

    case i of
        Left _ -> fail "Errored communicating with Redis"
        Right True  -> return $ "localhost:3000/" `mappend`
                                TL.decodeUtf8 (BL.fromStrict k)
        Right False -> createUrlFromString conn url

getRandomKey :: IO B.ByteString
getRandomKey = do
    s <- loop 5 []
    return $ B.pack s
  where loop :: Int -> [Word8] -> IO [Word8]
        loop 0 acc = return acc
        loop n acc = do
          c <- randomRIO (toWord8 'a', toWord8 'z')
          loop (n - 1) (c:acc)

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord
