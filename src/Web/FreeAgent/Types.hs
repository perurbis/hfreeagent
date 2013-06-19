{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Web.FreeAgent.Types where

import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import           Data.Data


type Token = BS.ByteString

data AccessTokenResp = AccessTokenResp {
       _access_token  :: BS.ByteString
     , _token_type    :: BS.ByteString
     , _expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)

$(deriveJSON tail ''AccessTokenResp)
