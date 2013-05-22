{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}



module Web.FreeAgent.Types where

import           Data.Aeson hiding (fromJSON)
import           Data.Aeson.Generic (fromJSON)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Data
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

type Token = BS.ByteString

data AccessTokenResp = AccessTokenResp {
       access_token  :: BS.ByteString
     , token_type    :: BS.ByteString
     , expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)

data DataDecl = DD {
     dataName :: String
   , fields :: [(String, String)]
} deriving Eq

instance Show DataDecl where
  show (DD name fields) = unlines $ [nameLine, fieldLines, derivingLine]
    where
      nameLine = "data " ++ name ++ " = " ++ name ++ " {"
      fieldLines = unlines . map fieldToLine $ zip [(0 :: Int)..] fields
      fieldToLine (0, (fname, ftype)) = "    " ++ fname ++ " :: " ++ ftype
      fieldToLine (_, (fname, ftype)) = "  , " ++ fname ++ " :: " ++ ftype
      derivingLine = "} deriving (Show, Data, Typeable)"
   
parseData :: String -> Value -> Maybe DataDecl
parseData name (Object o) = return $ DD name fields
  where
    fields = reverse $ HMS.foldrWithKey getFields [] o
    getFields k v seed = (T.unpack k, parseField v) : seed
parseData _ _ = Nothing
    
parseField :: Value -> String
parseField (String _) = "BS.ByteString"
parseField (Array _)  = "[SubObject]"
parseField (Object _) = "SubObject"
parseField (Number _) = "Double"
parseField (Bool _)   = "Bool"
parseField Null       = "Maybe a"

test :: Data a => FilePath -> T.Text -> IO (Maybe a)
test fname accessor = do
  raw <- BL.readFile fname
  case decode raw of
    Just jsonVal ->
         return $ parseMaybe (.: accessor) jsonVal >>= toType
    _ -> return Nothing
    
testCompany :: IO (Maybe Company)
testCompany = test "json/company.json" "company"

toType :: Data a => Value -> Maybe a
toType val = do
  case fromJSON val of
    Success a -> Just a
    _ -> Nothing
    
data Company = Company {
    subdomain :: BS.ByteString
  -- , type :: BS.ByteString
  , mileage_units :: BS.ByteString
  , freeagent_start_date :: BS.ByteString
  , first_accounting_year_end :: BS.ByteString
  , sales_tax_registration_status :: BS.ByteString
  , currency :: BS.ByteString
  , company_registration_number :: BS.ByteString
  , company_start_date :: BS.ByteString
  , url :: BS.ByteString
  , sales_tax_registration_number :: BS.ByteString
  , name :: BS.ByteString

} deriving (Show, Data, Typeable)