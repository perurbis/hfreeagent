{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.FreeAgent.Types where

import qualified Data.ByteString.Char8     as BS
import           Data.Data


type Token = BS.ByteString

data AccessTokenResp = AccessTokenResp {
       access_token  :: BS.ByteString
     , token_type    :: BS.ByteString
     , expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)