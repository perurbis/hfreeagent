{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}



module Web.FreeAgent.Types where

import           Data.Aeson.Generic ()
import qualified Data.ByteString.Char8           as BS
import           Data.Data

type Token = BS.ByteString

data AccessTokenResp = AccessTokenResp {
       access_token  :: BS.ByteString
     , token_type    :: BS.ByteString
     , expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)
     