{-# LANGUAGE OverloadedStrings #-}
module Controllers.Urls
    ( loadRoutes
    , handleUrlCreate
    , handleUrlView
    )
  where

import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as B hiding (unpack)
import qualified Data.ByteString.Char8   as B (unpack)
import qualified Data.ByteString.Lazy    as BL (fromStrict, toStrict)
import           Data.Char               (ord)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Word               (Word8)
import qualified Database.Redis          as Redis
import           Network.HTTP.Types      (status400, status404)
import           System.Random
import           Text.Hastache           (MuType (..))
import           Web.Scotty.Hastache     (ActionH', ScottyH', hastache, setH)
import           Web.Scotty.Trans        (get, param, post, redirect, status,
                                          text)

loadRoutes :: T.Text -> Redis.Connection -> ScottyH' ()
loadRoutes hostname conn = do
    get  "/:url_hash" (handleUrlRedirect conn)

    post "/urls"           (handleUrlCreate conn)
    get  "/urls/:url_hash" (handleUrlView hostname conn)

-- Routes
-------------------------------------------------------------------------------

-- |
-- GET /:url_hash
handleUrlRedirect :: Redis.Connection -> ActionH' ()
handleUrlRedirect conn = do
    k  <- param "url_hash"
    mr <- liftIO $ getUrlFromHash conn k
    case mr of
        Just r -> do
            liftIO $ incrementViewsFromHash conn k
            redirect r
        Nothing -> status status404 >> text "Invalid URL"

-- |
-- POST /urls
handleUrlCreate :: Redis.Connection -> ActionH' ()
handleUrlCreate conn = do
    url <- param "url"
    if validateUrl url
        then do
            hash <- liftIO $ getHashFromUrl conn url
            redirect $ "/urls/" <> hash
        else status status400 >> text "Invalid URL"

-- |
-- GET /urls/:url_hash
handleUrlView :: T.Text -> Redis.Connection -> ActionH' ()
handleUrlView hostname conn = do
    k <- param "url_hash"
    u <- liftIO $ getUrlFromHash conn k
    v <- liftIO $ getViewsFromHash conn k
    setH "original_url"  (MuVariable u)
    setH "shortened_url" (MuVariable (hostname <> "/" <> TL.toStrict k))
    setH "n_views" (MuVariable v)
    hastache "urls/view.html"

-- Utility functions
-------------------------------------------------------------------------------

incrementViewsFromHash :: Redis.Connection -> TL.Text -> IO ()
incrementViewsFromHash conn k = void $ Redis.runRedis conn $
    Redis.incr ("views::" <> BL.toStrict (TL.encodeUtf8 k))

validateUrl :: B.ByteString -> Bool
validateUrl u = not ("views::" `B.isPrefixOf` u)

getViewsFromHash :: Redis.Connection -> TL.Text -> IO Int
getViewsFromHash conn k = do
    rd <- Redis.runRedis conn $
        Redis.get ("views::" <> BL.toStrict (TL.encodeUtf8 k))
    case rd of
        Left _ -> fail "Error-ed communicating with Redis"
        Right (Just s) -> return (read (B.unpack s))
        Right Nothing -> return 0

getUrlFromHash :: Redis.Connection -> TL.Text -> IO (Maybe TL.Text)
getUrlFromHash conn k = do
    rd <- Redis.runRedis conn $ Redis.get $ BL.toStrict $ TL.encodeUtf8 k
    case rd of
        Left _ -> fail "Error-ed communicating with Redis"
        Right (Just s) -> return $ Just $ TL.decodeUtf8 $ BL.fromStrict s
        Right Nothing -> return Nothing

getHashFromUrl :: Redis.Connection -> B.ByteString -> IO TL.Text
getHashFromUrl conn url = do
    k <- getRandomKey
    i <- Redis.runRedis conn $ Redis.setnx k url

    case i of
        Left _ -> fail "Errored communicating with Redis"
        Right True  -> return $ TL.decodeUtf8 (BL.fromStrict k)
        Right False -> getHashFromUrl conn url

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
