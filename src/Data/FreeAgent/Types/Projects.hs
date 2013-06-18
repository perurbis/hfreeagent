{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Projects where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Project = Project {
    _hours_per_day                 :: BS.ByteString -- 8.0
  , _name                          :: BS.ByteString -- Test Project
  , _created_at                    :: BS.ByteString -- 2011-09-14T16:05:57Z
  , _status                        :: BS.ByteString -- Active
  , _normal_billing_rate           :: BS.ByteString -- 0.0
  , _contact                       :: BS.ByteString -- /contacts/1
  , _currency                      :: BS.ByteString -- GBP
  , _billing_period                :: BS.ByteString -- hour
  , _budget_units                  :: BS.ByteString -- Hours
  , _uses_project_invoice_sequence :: Bool
  , _budget                        :: Double
  , _is_ir35                       :: Bool
  , _updated_at                    :: BS.ByteString -- 2011-09-14T16:05:57Z

} deriving (Show, Data, Typeable)

type Projects = [ Project ]

instance FromJSON Project where
  parseJSON (Object v) = Project <$>
                     v .: "hours_per_day" <*>
                     v .: "name" <*>
                     v .: "created_at" <*>
                     v .: "status" <*>
                     v .: "normal_billing_rate" <*>
                     v .: "contact" <*>
                     v .: "currency" <*>
                     v .: "billing_period" <*>
                     v .: "budget_units" <*>
                     v .: "uses_project_invoice_sequence" <*>
                     v .: "budget" <*>
                     v .: "is_ir35" <*>
                     v .: "updated_at"

  parseJSON _            = empty




$(deriveToJSON tail ''Project)
$(makeLenses      ''Project)
