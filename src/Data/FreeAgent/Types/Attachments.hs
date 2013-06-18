{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Attachments where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Attachment = Attachment {
    _url          :: BS.ByteString -- https://api.freeagent.com/v2/attachments/3
  , _content_src  :: BS.ByteString -- https://s3.amazonaws.com/freeagent-dev/attachments/1/original.png?AWSAccessKeyId=1K3MW21E6T8KWBY84B02&Expires=1314281186&Signature=GFAKDo%2Bi%2FsUMTYEgg6ZWGysB4k4%3D
  , _content_type :: BS.ByteString -- image/png
  , _file_name    :: BS.ByteString -- barcode.png
  , _file_size    :: Double

} deriving (Show, Data, Typeable)


instance FromJSON Attachment where
  parseJSON (Object v) = Attachment <$>
                     v .: "url" <*>
                     v .: "content_src" <*>
                     v .: "content_type" <*>
                     v .: "file_name" <*>
                     v .: "file_size"

  parseJSON _            = empty



$(deriveToJSON tail ''Attachment)
$(makeLenses      ''Attachment)
