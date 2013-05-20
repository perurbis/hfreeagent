{-# LANGUAGE OverloadedStrings #-}
module Main where

--import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Blaze.ByteString.Builder as Builder (fromByteString, toByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8g
import           Data.Monoid
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as S
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

import           Web.FreeAgent.Types
--import qualified Data.ByteString.Char8 as B
--import Control.Monad.Maybe

faDocsUrl, faDocsHost :: C8.ByteString
faDocsUrl = "https://" `mappend` faDocsHost
faDocsHost = "dev.freeagent.com"

getUrl :: C8.ByteString -> IO (InputStream C8.ByteString)
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
              S.fromGenerator . S.yield $ Builder.toByteString x

getLinks :: C8.ByteString -> IO [C8.ByteString]
getLinks doc' = fmap (map C8.pack) links
  --doc <- getUrl "/docs" >>= return . parseHtml
  where doc = parseHtml $ C8.unpack doc'
        links = runX $ doc >>> css ".subnav a" ! "href"
  
docLinks :: IO (InputStream C8.ByteString)
docLinks = do
  doc <- getUrl "/docs"
  links <- S.mapM getLinks doc
  S.concatLists links

-- getJsonBlocks :: C8.ByteString -> IO [BL.ByteString]
-- getJsonBlocks url = do
--   doc <- getUrl url
--   jsonBlocks <- runX $ parseHtml doc >>> css "code.json" //> getText
--   return $ map BL.pack jsonBlocks

-- allJsonBlocks :: IO [BL.ByteString]
-- allJsonBlocks = getLinks >>= mapM getJsonBlocks >>= return . concat
main = do
  links <- docLinks
  out <- S.unlines S.stdout
  S.connect links out