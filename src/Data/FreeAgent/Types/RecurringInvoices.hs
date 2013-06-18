{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.RecurringInvoices where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

type InvoiceItems = [ InvoiceItem ]
data InvoiceItem = InvoiceItem {
    _url            :: BS.ByteString -- https://api.freeagent.com/v2/invoice_items/1
  , _item_type      :: BS.ByteString -- Hours
  , _description    :: BS.ByteString -- Item
  , _category       :: BS.ByteString -- https://api.freeagent.com/v2/categories/001
  , _sales_tax_rate :: BS.ByteString -- 20.0
  , _quantity       :: BS.ByteString -- 1.0
  , _position       :: Double
  , _price          :: BS.ByteString -- 2.0

} deriving (Show, Data, Typeable)



instance FromJSON InvoiceItem where
  parseJSON (Object v) = InvoiceItem <$>
                     v .: "url" <*>
                     v .: "item_type" <*>
                     v .: "description" <*>
                     v .: "category" <*>
                     v .: "sales_tax_rate" <*>
                     v .: "quantity" <*>
                     v .: "position" <*>
                     v .: "price"

  parseJSON _            = empty



$(deriveToJSON tail ''InvoiceItem)
$(makeLenses      ''InvoiceItem)
