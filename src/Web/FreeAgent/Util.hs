{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Web.FreeAgent.Util where

import           Control.Applicative
import           Control.Error.Util
import           Control.Monad                 (liftM, (>=>))
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text                     as T
import           Network.Http.Client
import           OpenSSL                       (withOpenSSL)
import           System.IO.Streams             (InputStream, stdout)
import           System.IO.Streams.Attoparsec
import           System.IO.Streams.Debug


toStrict :: BLI.ByteString -> C8.ByteString
toStrict =  BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict bs | BS.null bs = BLI.Empty
              | otherwise = BLI.Chunk bs BLI.Empty

baseFAHost, baseFAUrl :: BS.ByteString
baseFAHost = "api.irisopenbooks.co.uk"
baseFAUrl = "/v2/"


newtype FAMonad m a = FAMonad { runFA :: EitherT String m a }
        deriving (Functor, Monad, MonadIO)

runFreeAgent :: FAMonad m a -> m (Either String a)
runFreeAgent = runEitherT . runFA

faRequest :: Method -> BS.ByteString -> BS.ByteString -> IO Request
faRequest meth path token = buildRequest $ do
  http meth $ baseFAUrl `BS.append` path
  setHostname baseFAHost 443
  setAccept "text/json"
  setHeader "Authorization" $ "Bearer " `BS.append` token

noteFA :: (MonadIO m) => String -> Maybe a -> FAMonad m a
noteFA msg = FAMonad . EitherT . liftM (note msg) . return

liftFA :: Monad m => Either String a -> FAMonad m a
liftFA = FAMonad . hoistEither

aesonHandler :: FromJSON a => Response -> InputStream BS.ByteString -> IO (Maybe a)
aesonHandler _ = parseJSONFromStream

debugAesonHandler :: Response -> InputStream BS.ByteString -> IO Value
debugAesonHandler _ = debugInput id "AESON-HANDLER" stdout >=> parseValueFromStream


parseJSONFromStream :: FromJSON a => InputStream C8.ByteString -> IO (Maybe a)
parseJSONFromStream = parseFromStream $ parseMaybe parseJSON <$> json'

parseValueFromStream :: InputStream BS.ByteString -> IO Value
parseValueFromStream = parseFromStream json'

extractVal :: (FromJSON a) => T.Text -> Value -> Parser a
extractVal key = withObject "extract" (.: key)

--apiCall :: Token -> IO (Maybe Object)
apiCall
  :: (FromJSON a, MonadIO m) =>
     C8.ByteString -> C8.ByteString -> FAMonad m a
apiCall token url = do

  resp <- liftIO . withOpenSSL $ do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx baseFAHost 443
    r <- faRequest GET url token
    sendRequest c r emptyBody
    val <- receiveResponse c $ const parseValueFromStream
    closeConnection c
    return $ parseEither (parseJSON) val
  FAMonad $ hoistEither resp
  --noteFA ("No object decoded for "++ C8.unpack url) resp --  decode . fromStrict $ resp

extractApiVal :: (MonadIO m, FromJSON a) => T.Text -> Value -> FAMonad m a
extractApiVal key = liftFA . parseEither (extractVal key)