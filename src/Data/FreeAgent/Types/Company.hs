{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Company where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

type TimelineItems = [ TimelineItem ]
data TimelineItem = TimelineItem {
    _dated_on    :: BS.ByteString -- 2013-05-31
  , _description :: BS.ByteString -- Corporation Tax, year ending 31 May 12
  , _is_personal :: Bool
  , _nature      :: BS.ByteString -- Submission Due

} deriving (Show, Data, Typeable)

data Company = Company {
    _name                          :: BS.ByteString -- My Company
  , _sales_tax_registration_number :: BS.ByteString -- 123456
  , _url                           :: BS.ByteString -- https://api.freeagent.com/v2/company
  , _company_start_date            :: BS.ByteString -- 2010-05-01
  , _company_registration_number   :: BS.ByteString -- 123456
  , _currency                      :: BS.ByteString -- GBP
  , _sales_tax_registration_status :: BS.ByteString -- Registered
  , _first_accounting_year_end     :: BS.ByteString -- 2011-05-01
  , _freeagent_start_date          :: BS.ByteString -- 2010-05-01
  , _mileage_units                 :: BS.ByteString -- miles
  , _type                          :: BS.ByteString -- UkLimitedCompany
  , _subdomain                     :: BS.ByteString -- mycompany

} deriving (Show, Data, Typeable)



instance FromJSON TimelineItem where
  parseJSON (Object v) = TimelineItem <$>
                     v .: "dated_on" <*>
                     v .: "description" <*>
                     v .: "is_personal" <*>
                     v .: "nature"

  parseJSON _            = empty

instance FromJSON Company where
  parseJSON (Object v) = Company <$>
                     v .: "name" <*>
                     v .: "sales_tax_registration_number" <*>
                     v .: "url" <*>
                     v .: "company_start_date" <*>
                     v .: "company_registration_number" <*>
                     v .: "currency" <*>
                     v .: "sales_tax_registration_status" <*>
                     v .: "first_accounting_year_end" <*>
                     v .: "freeagent_start_date" <*>
                     v .: "mileage_units" <*>
                     v .: "type" <*>
                     v .: "subdomain"

  parseJSON _            = empty



$(deriveToJSON tail ''TimelineItem)
$(makeLenses      ''TimelineItem)
$(deriveToJSON tail ''Company)
$(makeLenses      ''Company)
