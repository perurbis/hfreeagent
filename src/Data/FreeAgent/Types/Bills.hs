{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Bills where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Bill = Bill {
    _dated_on        :: BS.ByteString -- 2011-09-14
  , _created_at      :: BS.ByteString -- 2011-09-14T16:00:41Z
  , _status          :: BS.ByteString -- Open
  , _category        :: BS.ByteString -- https://api.freeagent.com/v2/categories/285
  , _attachment      :: Attachment
  , _contact         :: BS.ByteString -- https://api.freeagent.com/v2/contacts/1
  , _total_value     :: BS.ByteString -- 100.0
  , _sales_tax_rate  :: BS.ByteString -- 20.0
  , _due_value       :: BS.ByteString -- 20.0
  , _paid_value      :: BS.ByteString -- 80.0
  , _reference       :: BS.ByteString -- sdasd
  , _sales_tax_value :: BS.ByteString -- -16.67
  , _updated_at      :: BS.ByteString -- 2011-09-14T16:00:41Z
  , _due_on          :: BS.ByteString -- 2011-10-14

} deriving (Show, Data, Typeable)

type Bills = [ Bill ]
data Attachment = Attachment {
    _url          :: BS.ByteString -- https://api.freeagent.com/v2/attachments/3
  , _content_src  :: BS.ByteString -- https://s3.amazonaws.com/freeagent-dev/attachments/1/original.png?AWSAccessKeyId=1K3MW21E6T8KWBY84B02&Expires=1314281186&Signature=GFAKDo%2Bi%2FsUMTYEgg6ZWGysB4k4%3D
  , _content_type :: BS.ByteString -- image/png
  , _file_name    :: BS.ByteString -- barcode.png
  , _file_size    :: Double

} deriving (Show, Data, Typeable)


instance FromJSON Bill where
  parseJSON (Object v) = Bill <$>
                     v .: "dated_on" <*>
                     v .: "created_at" <*>
                     v .: "status" <*>
                     v .: "category" <*>
                     v .: "attachment" <*>
                     v .: "contact" <*>
                     v .: "total_value" <*>
                     v .: "sales_tax_rate" <*>
                     v .: "due_value" <*>
                     v .: "paid_value" <*>
                     v .: "reference" <*>
                     v .: "sales_tax_value" <*>
                     v .: "updated_at" <*>
                     v .: "due_on"

  parseJSON _            = empty


instance FromJSON Attachment where
  parseJSON (Object v) = Attachment <$>
                     v .: "url" <*>
                     v .: "content_src" <*>
                     v .: "content_type" <*>
                     v .: "file_name" <*>
                     v .: "file_size"

  parseJSON _            = empty



$(deriveToJSON tail ''Bill)
$(makeLenses      ''Bill)
$(deriveToJSON tail ''Attachment)
$(makeLenses      ''Attachment)
