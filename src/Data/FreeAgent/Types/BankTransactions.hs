{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.BankTransactions where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

type BankTransactions = [ BankTransaction ]
data BankTransaction = BankTransaction {
    _dated_on           :: BS.ByteString -- 2010-06-14
  , _url                :: BS.ByteString -- https://api.freeagent.com/v2/bank_transactions/15
  , _description        :: BS.ByteString -- .Ledner Inc/.mesh enterprise platforms (246)942-9558
  , _bank_account       :: BS.ByteString -- https://api.freeagent.com/v2/bank_accounts/1
  , _amount             :: BS.ByteString -- -350.0
  , _unexplained_amount :: BS.ByteString -- 0.0
  , _is_manual          :: Bool

} deriving (Show, Data, Typeable)



instance FromJSON BankTransaction where
  parseJSON (Object v) = BankTransaction <$>
                     v .: "dated_on" <*>
                     v .: "url" <*>
                     v .: "description" <*>
                     v .: "bank_account" <*>
                     v .: "amount" <*>
                     v .: "unexplained_amount" <*>
                     v .: "is_manual"

  parseJSON _            = empty



$(deriveToJSON tail ''BankTransaction)
$(makeLenses      ''BankTransaction)
