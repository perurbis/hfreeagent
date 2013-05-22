{-# LANGUAGE OverloadedStrings #-}
module Main where

--import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Blaze.ByteString.Builder   as Builder (fromByteString, toByteString)
import           Control.Monad              ((>=>))
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Char                  (toUpper)
import qualified Data.HashMap.Strict        as HMS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as S
import           System.IO.Streams.Debug
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

import           Web.FreeAgent.Types

-- import           System.IO.Streams.Debug
--import qualified Data.ByteString.Char8 as B
--import Control.Monad.Maybe

faDocsUrl, faDocsHost :: C8.ByteString
faDocsUrl = "https://" `mappend` faDocsHost
faDocsHost = "dev.freeagent.com"

type Url = C8.ByteString
type Key = String

getUrl :: Url -> IO C8.ByteString
getUrl url = withOpenSSL $ do
    ctx <- baselineContextSSL
    let c = openConnectionSSL ctx faDocsHost 443
    r <- req
    withConnection c $ \con -> do
        sendRequest con r emptyBody
        receiveResponse con handle
    where
          req = buildRequest $ do
              http GET $ faDocsUrl `BS.append` url
              setHostname faDocsHost 443
          handle _ input = do
              i2 <- S.map Builder.fromByteString input
              x <- S.fold mappend mempty i2
              return $ Builder.toByteString x

docLinks :: IO (InputStream Url)
docLinks = doExtract getLinks "/docs"

doExtract
  :: (C8.ByteString -> IO [a]) -> Url -> IO (InputStream a)
doExtract action = getUrl >=> action >=> S.fromList

getJsonBlocks :: C8.ByteString -> IO [Maybe Value]
getJsonBlocks doc' = fmap (map extractJson) jsonBlocks
  where doc = parseHtml $ C8.unpack doc'
        jsonBlocks = runX $ doc >>> css "code" //> getText
        extractJson :: String -> Maybe Value
        extractJson = decode' . LC8.pack


getJsonTypes :: Value -> [(Key, Maybe DataDecl)]
getJsonTypes (Object hm) = [(T.unpack k, parseData (capStr k) v) | (k, v) <- HMS.toList hm]
  where
        capStr :: T.Text -> String
        capStr txt = toUpper (T.head txt) : (T.unpack $ T.tail txt)
getJsonTypes (Array arr) = getJsonTypes $ V.head arr
getJsonTypes _ = []

-- allJsonBlocks :: IO [BL.ByteString]
-- allJsonBlocks = getLinks >>= mapM getJsonBlocks >>= return . concat

getLinks :: C8.ByteString -> IO [C8.ByteString]
getLinks doc' = fmap (map C8.pack) links
  --doc <- getUrl "/docs" >>= return . parseHtml
  where doc = parseHtml $ C8.unpack doc'
        links = runX $ doc >>> css ".subnav a" ! "href"


main :: IO ()
main = do
  links <- docLinks >>= debugInput id "LINKS" S.stdout
  -- jsonObjs <-  >>= getJson
  jsonObjs <- getDataDecls =<< getJson links
  out <- S.unlines S.stdout
  firstFew <- S.take 20 jsonObjs
  ins <- S.map (C8.pack . show) firstFew
  --S.connect ins out
  -- S.connect links  out
  print =<< fmap length (S.toList firstFew)

getDataDecls
  :: InputStream Value -> IO (InputStream (Key, Maybe DataDecl))
getDataDecls = S.map getJsonTypes >=> debugInput (C8.pack . show) "TYPES" S.stdout >=> S.concatLists

getJson :: InputStream Url -> IO (InputStream Value)
getJson = S.mapM (getUrl >=> getJsonBlocks) >=> debugInput lenMaybes "JSON-BLOCKS" S.stdout >=> S.map catMaybes >=> S.concatLists

lenMaybes :: [Maybe a] -> C8.ByteString
lenMaybes ml = C8.pack $ (show (length $ catMaybes ml)) ++ "/" ++ (show $ length ml)
