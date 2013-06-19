{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Data.FreeAgent.Types.Categories where

import           Control.Applicative (empty, (<$>), (<*>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString     as BS
import           Data.Data
import qualified Data.Map            as M

data TransactionCategory = TransactionCategory {
    _url                 :: BS.ByteString -- https://api.freeagent.com/v2/categories/001
  , _description         :: BS.ByteString -- Sales
  , _nominal_code        :: BS.ByteString -- 001
  , _auto_sales_tax_rate :: Maybe BS.ByteString -- Standard rate
  , _tax_reporting_name  :: Maybe BS.ByteString

} deriving (Show, Data, Typeable)

type Categories = [TransactionCategory]

data TransactionTypes = TransactionTypes {
    _admin_expenses_categories :: Categories
  , _cost_of_sales_categories :: Categories
  , _income_categories :: Categories
  , _general_categories :: Categories
} deriving (Show, Data, Typeable)

type CatMap = M.Map BS.ByteString TransactionCategory

instance FromJSON TransactionCategory where
  parseJSON (Object v) = TransactionCategory <$>
                     v .:  "url" <*>
                     v .:  "description" <*>
                     v .:  "nominal_code" <*>
                     v .:? "auto_sales_tax_rate" <*>
                     v .
                     
                     :? "tax_reporting_name"

  parseJSON _            = empty


$(deriveToJSON tail ''TransactionCategory)
$(makeLenses      ''TransactionCategory)

$(deriveJSON tail ''TransactionTypes)
$(makeLenses      ''TransactionTypes)
