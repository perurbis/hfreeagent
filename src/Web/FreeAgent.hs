{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.FreeAgent where

import           Control.Monad.IO.Class          (MonadIO)
import qualified Data.ByteString.Char8           as C8
import           Data.Data

import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient


import           Web.FreeAgent.OAuth2
import           Web.FreeAgent.Types
import           Web.FreeAgent.Util
-- a custom request


getCompany
  :: (Data.Data.Data b, MonadIO m) => C8.ByteString -> FAMonad m b
getCompany = apiCall "company"


getAccessToken :: IO Token
getAccessToken = do
  print $ authorizationUrl faKey `appendQueryParam'` faTokenParams
  putStrLn "visit the url and paste code here: "
  code <- fmap C8.pack getLine
  (Just (AccessToken accessToken Nothing)) <- requestAccessToken faKey code
  print accessToken
  return accessToken
