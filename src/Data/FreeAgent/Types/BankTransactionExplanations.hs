{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.BankTransactionExplanations where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data BankTransactionExplanation = BankTransactionExplanation {
    _dated_on         :: BS.ByteString -- 2010-05-01
  , _description      :: BS.ByteString -- harness end-to-end e-business
  , _category         :: BS.ByteString -- https://api.freeagent.com/v2/categories/285
  , _attachment       :: Attachment
  , _bank_account     :: BS.ByteString -- https://api.freeagent.com/v2/bank_accounts/1
  , _gross_value      :: BS.ByteString -- -730.0
  , _bank_transaction :: BS.ByteString -- https://api.freeagent.com/v2/bank_transactions/8

} deriving (Show, Data, Typeable)

data Attachment = Attachment {
    _url          :: BS.ByteString -- https://api.freeagent.com/v2/attachments/3
  , _content_src  :: BS.ByteString -- https://s3.amazonaws.com/freeagent-dev/attachments/1/original.png?AWSAccessKeyId=1K3MW21E6T8KWBY84B02&Expires=1314281186&Signature=GFAKDo%2Bi%2FsUMTYEgg6ZWGysB4k4%3D
  , _content_type :: BS.ByteString -- image/png
  , _file_name    :: BS.ByteString -- barcode.png
  , _file_size    :: Double

} deriving (Show, Data, Typeable)

type BankTransactionExplanations = [ BankTransactionExplanation ]

instance FromJSON BankTransactionExplanation where
  parseJSON (Object v) = BankTransactionExplanation <$>
                     v .: "dated_on" <*>
                     v .: "description" <*>
                     v .: "category" <*>
                     v .: "attachment" <*>
                     v .: "bank_account" <*>
                     v .: "gross_value" <*>
                     v .: "bank_transaction"

  parseJSON _            = empty

instance FromJSON Attachment where
  parseJSON (Object v) = Attachment <$>
                     v .: "url" <*>
                     v .: "content_src" <*>
                     v .: "content_type" <*>
                     v .: "file_name" <*>
                     v .: "file_size"

  parseJSON _            = empty




$(deriveToJSON tail ''BankTransactionExplanation)
$(makeLenses      ''BankTransactionExplanation)
$(deriveToJSON tail ''Attachment)
$(makeLenses      ''Attachment)
