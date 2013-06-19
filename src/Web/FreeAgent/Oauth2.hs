{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.FreeAgent.OAuth2 where



import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C8
import           Network.Http.Client
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient
import           OpenSSL                         (withOpenSSL)

import           Web.FreeAgent.Types
import           Web.FreeAgent.Util


freeAgentID, freeAgentSecret, faToken :: BS.ByteString
freeAgentID = "iumpnreVbwSdd6zhAtHVXw"
freeAgentSecret = "jJsH80b3SlAB9c8my72mtg"
faToken = "1Uw9iAopXYITVISQoiX08jf7QhrieqG7NFAaJnYyP"

-- Returned tokens
faRefreshToken :: Token
faRefreshToken = "1VQZFuqGZB9nUk7pbavLtHX11K0Dp8OafvFtTctul"

faKey :: OAuth2
faKey = OAuth2 { oauthClientId            = freeAgentID
               , oauthClientSecret        = freeAgentSecret
               , oauthCallback            = Nothing
               , oauthOAuthorizeEndpoint  = "https://api.irisopenbooks.co.uk/v2/approve_app"
               , oauthAccessTokenEndpoint = "https://api.irisopenbooks.co.uk/v2/token_endpoint"
               , oauthAccessToken         = Nothing
               }

refreshTokenM :: Monad m =>  FAMonad m Token
refreshTokenM = return faRefreshToken

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
  resp <- receiveResponse c aesonHandler
  closeConnection c
  return resp

getToken :: Token -> FAMonad IO Token
getToken token = do
  t <- liftIO $ refreshAccessToken faKey token
  -- return $ case t of
  --   Just (AccessToken tk _) -> Right tk
  --   Nothing -> Left "No Token"
  noteFA "No token returned" t >>= \(AccessToken tk _) -> return tk

faTokenParams :: QueryParams
faTokenParams = [("response_type", "code"), ("client_id", "565")]

getAccessToken :: IO Token
getAccessToken = do
  print $ authorizationUrl faKey `appendQueryParam'` faTokenParams
  putStrLn "visit the url and paste code here: "
  code <- fmap C8.pack getLine
  (Just (AccessToken accessToken Nothing)) <- requestAccessToken faKey code
  print accessToken
  return accessToken
