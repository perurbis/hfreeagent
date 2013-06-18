{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Timeslips where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

type Timeslips = [ Timeslip ]
data Timeslip = Timeslip {
    _project    :: BS.ByteString -- https://api.freeagent.com/v2/projects/1
  , _hours      :: BS.ByteString -- 12.0
  , _dated_on   :: BS.ByteString -- 2011-08-14
  , _created_at :: BS.ByteString -- 2011-08-16T13:32:00Z
  , _user       :: BS.ByteString -- https://api.freeagent.com/v2/users/1
  , _updated_at :: BS.ByteString -- 2011-08-16T13:32:00Z
  , _task       :: BS.ByteString -- https://api.freeagent.com/v2/tasks/1

} deriving (Show, Data, Typeable)



instance FromJSON Timeslip where
  parseJSON (Object v) = Timeslip <$>
                     v .: "project" <*>
                     v .: "hours" <*>
                     v .: "dated_on" <*>
                     v .: "created_at" <*>
                     v .: "user" <*>
                     v .: "updated_at" <*>
                     v .: "task"

  parseJSON _            = empty



$(deriveToJSON tail ''Timeslip)
$(makeLenses      ''Timeslip)
