{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.QuickStart where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Company = Company {
    _company_start_date            :: BS.ByteString -- 2010-07-01
  , _currency                      :: BS.ByteString -- GBP
  , _sales_tax_registration_status :: BS.ByteString -- Registered
  , _mileage_units                 :: BS.ByteString -- miles
  , _type                          :: BS.ByteString -- UkLimitedCompany

} deriving (Show, Data, Typeable)


instance FromJSON Company where
  parseJSON (Object v) = Company <$>
                     v .: "company_start_date" <*>
                     v .: "currency" <*>
                     v .: "sales_tax_registration_status" <*>
                     v .: "mileage_units" <*>
                     v .: "type"

  parseJSON _            = empty



$(deriveToJSON tail ''Company)
$(makeLenses      ''Company)
