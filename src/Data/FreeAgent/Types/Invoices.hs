{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Data.FreeAgent.Types.Invoices where

import           Control.Applicative (empty, (<$>), (<*>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString     as BS
import           Data.Data

type InvoiceTimelineItems = [ InvoiceTimelineItem ]
data InvoiceTimelineItem = InvoiceTimelineItem {
  --   _dated_on    :: BS.ByteString -- 2011-09-02
  -- , _description :: BS.ByteString -- resras
    _summary     :: BS.ByteString -- Payment: 007: £14.40 received
  , _amount      :: BS.ByteString -- 14.4
  -- , _reference   :: BS.ByteString -- 007

} deriving (Show, Data, Typeable)

type InvoiceItems = [ InvoiceItem ]
data InvoiceItem = InvoiceItem {
    _item_type   :: BS.ByteString -- Hours
  , _description :: BS.ByteString -- Test InvoiceItem
  , _quantity    :: BS.ByteString -- 0.0
  , _price       :: BS.ByteString -- 0.0

} deriving (Show, Data, Typeable)

data Invoice = Invoice {
    _omit_header           :: Bool
  , _dated_on              :: BS.ByteString -- 2001-12-12T00:00:00+00:00
  , _status                :: BS.ByteString -- Draft
  , _contact               :: BS.ByteString -- https://api.freeagent.com/v2/contacts/2
  , _currency              :: BS.ByteString -- GBP
  , _invoice_items         :: [ InvoiceItem ]
  , _exchange_rate         :: BS.ByteString -- 1.0
  , _payment_terms_in_days :: Double
  , _net_value             :: BS.ByteString -- 0.0
  , _reference             :: BS.ByteString -- 003
  , _due_on                :: BS.ByteString -- 2001-12-17T00:00:00+00:00

} deriving (Show, Data, Typeable)



instance FromJSON InvoiceTimelineItem where
  parseJSON (Object v) = InvoiceTimelineItem <$>
                     -- v .: "dated_on" <*>
                     -- v .: "description" <*>
                     v .: "summary" <*>
                     v .: "amount" -- <*>
                     -- v .: "reference"

  parseJSON _            = empty


instance FromJSON InvoiceItem where
  parseJSON (Object v) = InvoiceItem <$>
                     v .: "item_type" <*>
                     v .: "description" <*>
                     v .: "quantity" <*>
                     v .: "price"

  parseJSON _            = empty

instance FromJSON Invoice where
  parseJSON (Object v) = Invoice <$>
                     v .: "omit_header" <*>
                     v .: "dated_on" <*>
                     v .: "status" <*>
                     v .: "contact" <*>
                     v .: "currency" <*>
                     v .: "invoice_items" <*>
                     v .: "exchange_rate" <*>
                     v .: "payment_terms_in_days" <*>
                     v .: "net_value" <*>
                     v .: "reference" <*>
                     v .: "due_on"

  parseJSON _            = empty



$(deriveToJSON tail ''InvoiceTimelineItem)
$(makeLenses      ''InvoiceTimelineItem)
$(deriveToJSON tail ''InvoiceItem)
$(makeLenses      ''InvoiceItem)
$(deriveToJSON tail ''Invoice)
$(makeLenses      ''Invoice)
