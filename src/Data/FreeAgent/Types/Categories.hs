{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Categories where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data IncomeCategories = IncomeCategories {
    _url                 :: BS.ByteString -- https://api.freeagent.com/v2/categories/001
  , _description         :: BS.ByteString -- Sales
  , _nominal_code        :: BS.ByteString -- 001
  , _auto_sales_tax_rate :: BS.ByteString -- Standard rate

} deriving (Show, Data, Typeable)


instance FromJSON IncomeCategories where
  parseJSON (Object v) = IncomeCategories <$>
                     v .: "url" <*>
                     v .: "description" <*>
                     v .: "nominal_code" <*>
                     v .: "auto_sales_tax_rate"

  parseJSON _            = empty



$(deriveToJSON tail ''IncomeCategories)
$(makeLenses      ''IncomeCategories)
