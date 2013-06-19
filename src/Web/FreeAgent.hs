{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Web.FreeAgent where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8             as C8
import           Data.FreeAgent.Types.BankAccounts
import           Data.FreeAgent.Types.Categories
import           Data.FreeAgent.Types.Company
import           Data.FreeAgent.Types.Expenses
import qualified Data.Map as M
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient
import           Web.FreeAgent.OAuth2
import           Web.FreeAgent.Types
import           Web.FreeAgent.Util


t :: Token
t = "1qct_y3hb_AzVzFu6S4ndNx6UDDZuHYALqLkm5T69"

data CompanyResp = CompanyResp { company :: Company }
  deriving Show
  
data ExpensesResp = ExpensesResp { expenses :: Expenses}

$(deriveJSON id ''CompanyResp)
$(deriveJSON id ''ExpensesResp)

getCompany :: (MonadIO m) => Token -> FAMonad m Company
getCompany t = apiCall t "company" >>= extractApiVal "company"

getBa :: (MonadIO m) => Token -> FAMonad m Value
getBa t = apiCall t "bank_accounts/207811"

getAccount :: FAMonad IO BankAccount
getAccount = getBa t >>= FAMonad . hoistEither . parseEither (extractVal "bank_account")

getExpenses :: MonadIO m => Token -> FAMonad m Expenses
getExpenses t = apiCall t "expenses" >>= extractApiVal "expenses"

getExps :: MonadIO m => Token -> FAMonad m ExpensesResp
getExps t = apiCall t "expenses"

getAccessToken :: IO Token
getAccessToken = do
  print $ authorizationUrl faKey `appendQueryParam'` faTokenParams
  putStrLn "visit the url and paste code here: "
  code <- fmap C8.pack getLine
  (Just (AccessToken accessToken Nothing)) <- requestAccessToken faKey code
  print accessToken
  return accessToken
  
-- getCategories :: MonadIO m => FAMonad m CatMap
getCategories = catMapTypes <$> apiCall t "categories"
    
catMapTypes :: TransactionTypes -> CatMap
catMapTypes (TransactionTypes admin cos income general) = M.fromList $ map extractKV allTypes
  where
    allTypes = concat [admin, cos, income, general]
    extractKV tt = (tt^.nominal_code, tt)

printExpenses = do
  (Right exp) <- runFreeAgent $ getExpenses t
  return $ map (\e -> (e^.gross_value, e^.category)) exp