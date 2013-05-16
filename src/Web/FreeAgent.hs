{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.FreeAgent where

import           Control.Error.Util
import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson.Generic
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Internal   as BLI
import           Data.Maybe
import           Network.Http.Client
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient
import           OpenSSL                         (withOpenSSL)
--import           System.IO.Streams               (InputStream, OutputStream, stdout)


import           Web.FreeAgent.Types
-- a custom request

toStrict :: BLI.ByteString -> C8.ByteString
toStrict =  BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict bs | BS.null bs = BLI.Empty
              | otherwise = BLI.Chunk bs BLI.Empty
freeAgentID, freeAgentSecret, faToken, baseFAHost, baseFAUrl :: BS.ByteString
freeAgentID = "iumpnreVbwSdd6zhAtHVXw"
freeAgentSecret = "jJsH80b3SlAB9c8my72mtg"
faToken = "1Uw9iAopXYITVISQoiX08jf7QhrieqG7NFAaJnYyP"

baseFAHost = "api.irisopenbooks.co.uk"
baseFAUrl = "/v2/"

-- Returned tokens
accessToken, faRefreshToken :: Token
accessToken = "1MZnkKxm6nVHMJy2cBMcbMHy95Lj8c1DMKvgPEZpo"
faRefreshToken = "1VQZFuqGZB9nUk7pbavLtHX11K0Dp8OafvFtTctul"

newtype FAMonad m a = FAMonad { runFA :: EitherT String m a }
        deriving (Monad, MonadIO)
        
runFreeAgent :: FAMonad m a -> m (Either String a)
runFreeAgent = runEitherT . runFA

faKey :: OAuth2
faKey = OAuth2 { oauthClientId            = freeAgentID
               , oauthClientSecret        = freeAgentSecret
               , oauthCallback            = Nothing
               , oauthOAuthorizeEndpoint  = "https://api.irisopenbooks.co.uk/v2/approve_app"
               , oauthAccessTokenEndpoint = "https://api.irisopenbooks.co.uk/v2/token_endpoint"
               , oauthAccessToken         = Nothing
               }

faTokenParams :: QueryParams
faTokenParams = [("response_type", "code"), ("client_id", "565")]


faRequest :: Method -> BS.ByteString -> BS.ByteString -> IO Request
faRequest meth path token = buildRequest $ do
  http meth $ baseFAUrl `BS.append` path
  setHostname baseFAHost 443
  setAccept "text/json"
  setHeader "Authorization" $ "Bearer " `BS.append` token

faRefreshRequest :: BS.ByteString -> IO Request
faRefreshRequest token = buildRequest $ do
  http POST $ baseFAUrl `BS.append` queryParams
  setHostname baseFAHost 443
  setAuthorizationBasic freeAgentID freeAgentSecret
  --setHeader "Content-Length" "0"
  setHeader "Transfer-Encoding" "none"
  --setAccept "text/json"
  where queryParams = "token_endpoint?grant_type=refresh_token&refresh_token=" `BS.append` token

refresh :: IO (Maybe AccessTokenResp)
refresh = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx baseFAHost 443
  r <- faRefreshRequest faRefreshToken
  sendRequest c r emptyBody
  resp <- receiveResponse c concatHandler
  closeConnection c
  return . decode . fromStrict $ resp

noteFA :: (MonadIO m) => String -> Maybe a -> FAMonad m a
noteFA msg = FAMonad . EitherT . liftM (note msg) . return

sendRecv :: Token -> IO C8.ByteString
sendRecv token = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx baseFAHost 443
  r <- faRequest GET "company" token
  sendRequest c r emptyBody
  resp <- receiveResponse c concatHandler
  closeConnection c
  return resp --  decode . fromStrict $ resp

getToken :: Token -> FAMonad IO Token
getToken token = do
  t <- liftIO $ refreshAccessToken faKey token
  -- return $ case t of
  --   Just (AccessToken tk _) -> Right tk
  --   Nothing -> Left "No Token"
  noteFA "No token returned" t >>= \(AccessToken tk _) -> return tk

getAccessToken :: IO Token
getAccessToken = do
  print $ authorizationUrl faKey `appendQueryParam'` faTokenParams
  putStrLn "visit the url and paste code here: "
  code <- fmap C8.pack getLine
  (Just (AccessToken accessToken Nothing)) <- requestAccessToken faKey code
  print accessToken
  return accessToken
