{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Tasks where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Task = Task {
    _project        :: BS.ByteString -- /projects/1
  , _name           :: BS.ByteString -- Sample Task
  , _created_at     :: BS.ByteString -- 2011-08-16T11:06:57Z
  , _status         :: BS.ByteString -- Active
  , _is_billable    :: Bool
  , _billing_period :: BS.ByteString -- hour
  , _billing_rate   :: BS.ByteString -- 0.0
  , _updated_at     :: BS.ByteString -- 2011-08-16T11:06:57Z

} deriving (Show, Data, Typeable)

type Tasks = [ Task ]

instance FromJSON Task where
  parseJSON (Object v) = Task <$>
                     v .: "project" <*>
                     v .: "name" <*>
                     v .: "created_at" <*>
                     v .: "status" <*>
                     v .: "is_billable" <*>
                     v .: "billing_period" <*>
                     v .: "billing_rate" <*>
                     v .: "updated_at"

  parseJSON _            = empty




$(deriveToJSON tail ''Task)
$(makeLenses      ''Task)
