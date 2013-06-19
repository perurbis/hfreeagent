{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Data.FreeAgent.Types.Expenses where

import           Control.Applicative (empty, (<$>), (<*>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString     as BS
import           Data.Data

data Expense = Expense {
    _dated_on                :: BS.ByteString -- 2011-08-24
  , _created_at              :: BS.ByteString -- 2011-08-24T08:10:40Z
  , _description             :: BS.ByteString -- Some description
  , _user                    :: BS.ByteString -- https://api.freeagent.com/v2/users/1
  , _category                :: BS.ByteString -- https://api.freeagent.com/v2/categories/285
  , _attachment              :: Maybe Attachment
  , _sales_tax_rate          :: Maybe BS.ByteString -- 20.0
  , _gross_value             :: BS.ByteString -- -12.0
  , _manual_sales_tax_amount :: Maybe BS.ByteString -- 0.12
  , _updated_at              :: BS.ByteString -- 2011-08-24T08:10:40Z

} deriving (Show, Data, Typeable)

type Expenses = [ Expense ]
data Attachment = Attachment {
    _url          :: BS.ByteString -- https://api.freeagent.com/v2/attachments/3
  , _content_src  :: BS.ByteString -- https://s3.amazonaws.com/freeagent-dev/attachments/1/original.png?AWSAccessKeyId=1K3MW21E6T8KWBY84B02&Expires=1314281186&Signature=GFAKDo%2Bi%2FsUMTYEgg6ZWGysB4k4%3D
  , _content_type :: BS.ByteString -- image/png
  , _file_name    :: BS.ByteString -- barcode.png
  , _file_size    :: Double

} deriving (Show, Data, Typeable)


instance FromJSON Expense where
  parseJSON (Object v) = Expense <$>
                     v .:  "dated_on" <*>
                     v .:  "created_at" <*>
                     v .:  "description" <*>
                     v .:  "user" <*>
                     v .:  "category" <*>
                     v .:? "attachment" <*>
                     v .:? "sales_tax_rate" <*>
                     v .:  "gross_value" <*>
                     v .:? "manual_sales_tax_amount" <*>
                     v .:  "updated_at"

  parseJSON _            = empty


instance FromJSON Attachment where
  parseJSON (Object v) = Attachment <$>
                     v .: "url" <*>
                     v .: "content_src" <*>
                     v .: "content_type" <*>
                     v .: "file_name" <*>
                     v .: "file_size"

  parseJSON _            = empty



$(deriveToJSON tail ''Expense)
$(makeLenses      ''Expense)
$(deriveToJSON tail ''Attachment)
$(makeLenses      ''Attachment)
