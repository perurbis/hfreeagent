{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.FreeAgent.Util where

import           Control.Error.Util
import           Control.Monad                 (liftM, (>=>), join)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson.Generic
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.Data
import           Data.Maybe
import           Network.Http.Client
import           OpenSSL                       (withOpenSSL)
import           System.IO.Streams             (InputStream)
import qualified System.IO.Streams             as S

import           Web.FreeAgent.Types
-- a custom request

toStrict :: BLI.ByteString -> C8.ByteString
toStrict =  BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict bs | BS.null bs = BLI.Empty
              | otherwise = BLI.Chunk bs BLI.Empty

baseFAHost, baseFAUrl :: BS.ByteString
baseFAHost = "api.irisopenbooks.co.uk"
baseFAUrl = "/v2/"


newtype FAMonad m a = FAMonad { runFA :: EitherT String m a }
        deriving (Monad, MonadIO)

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

aesonHandler :: (Data a) => Response -> InputStream BS.ByteString -> IO (Maybe a)
aesonHandler _ =  S.map decode'Strict >=> S.read >=> return . join
  where decode'Strict = decode' . fromStrict
  
--apiCall :: Token -> IO (Maybe Object)
apiCall url token = do
  
  resp <- liftIO . withOpenSSL $ do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx baseFAHost 443
    r <- faRequest GET url token
    sendRequest c r emptyBody
    resp <- receiveResponse c aesonHandler
    closeConnection c
    return resp
  noteFA ("No object decoded for "++ C8.unpack url) resp --  decode . fromStrict $ resp
