{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.BankAccounts where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data BankAccount = BankAccount {
    _name            :: BS.ByteString -- Default bank account
  , _created_at      :: BS.ByteString -- 2011-07-28T11:25:11Z
  , _is_personal     :: Bool
  , _type            :: BS.ByteString -- StandardBankAccount
  , _opening_balance :: BS.ByteString -- 0.0
  , _updated_at      :: BS.ByteString -- 2011-07-28T11:25:20Z

} deriving (Show, Data, Typeable)

type BankAccounts = [ BankAccount ]

instance FromJSON BankAccount where
  parseJSON (Object v) = BankAccount <$>
                     v .: "name" <*>
                     v .: "created_at" <*>
                     v .: "is_personal" <*>
                     v .: "type" <*>
                     v .: "opening_balance" <*>
                     v .: "updated_at"

  parseJSON _            = empty




$(deriveToJSON tail ''BankAccount)
$(makeLenses      ''BankAccount)
