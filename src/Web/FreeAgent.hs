{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.FreeAgent where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C8
import           Network.Http.Client
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient
import           OpenSSL                         (withOpenSSL)
--import           System.IO.Streams               (InputStream, OutputStream, stdout)


import           Web.FreeAgent.OAuth2
import           Web.FreeAgent.Types
import           Web.FreeAgent.Util
-- a custom request


sendRecv :: Token -> IO (Maybe Object)
sendRecv token = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx baseFAHost 443
  r <- faRequest GET "company" token
  sendRequest c r emptyBody
  resp <- receiveResponse c aesonHandler
  closeConnection c
  return resp --  decode . fromStrict $ resp


getAccessToken :: IO Token
getAccessToken = do
  print $ authorizationUrl faKey `appendQueryParam'` faTokenParams
  putStrLn "visit the url and paste code here: "
  code <- fmap C8.pack getLine
  (Just (AccessToken accessToken Nothing)) <- requestAccessToken faKey code
  print accessToken
  return accessToken
