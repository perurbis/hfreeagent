{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Contacts where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Contact = Contact {
    _contact_name_on_invoices      :: Bool
  , _created_at                    :: BS.ByteString -- 2011-09-14T16:00:41Z
  , _last_name                     :: BS.ByteString -- me
  , _locale                        :: BS.ByteString -- en
  , _uses_contact_invoice_sequence :: Bool
  , _account_balance               :: BS.ByteString -- -100.0
  , _updated_at                    :: BS.ByteString -- 2011-09-16T09:34:41Z
  , _first_name                    :: BS.ByteString -- test
  , _country                       :: BS.ByteString -- United Kingdom

} deriving (Show, Data, Typeable)

type Contacts = [ Contact ]

instance FromJSON Contact where
  parseJSON (Object v) = Contact <$>
                     v .: "contact_name_on_invoices" <*>
                     v .: "created_at" <*>
                     v .: "last_name" <*>
                     v .: "locale" <*>
                     v .: "uses_contact_invoice_sequence" <*>
                     v .: "account_balance" <*>
                     v .: "updated_at" <*>
                     v .: "first_name" <*>
                     v .: "country"

  parseJSON _            = empty




$(deriveToJSON tail ''Contact)
$(makeLenses      ''Contact)
