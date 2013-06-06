{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Blaze.ByteString.Builder   as Builder (fromByteString, toByteString)
import           Control.Monad              (forM_, forM, mapM_, (>=>))
import           Control.Monad.Trans.State (execStateT, evalStateT)
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Maybe
import           Data.Monoid
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import           System.Directory
import           System.FilePath            ((</>))
import           System.IO
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as S
import           System.IO.Streams.Debug
import           System.IO.Streams.File
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

import           Data.FreeAgent.Generate

faDocsUrl, faDocsHost :: C8.ByteString
faDocsUrl = "https://" `mappend` faDocsHost
faDocsHost = "dev.freeagent.com"

type Url = C8.ByteString

-- | Get the contents of a webpage from a url
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

-- | Get a list of links to process from FreeAgent's homepage
docLinks :: IO (InputStream Url)
docLinks = getUrl >=> getLinks >=> S.fromList $ "/docs"

-- | Get a list of links to process from locally stored FreeAgent pages
docLinksLocal :: FilePath -> IO [FilePath]
docLinksLocal topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  return $ map (topdir </>) properNames


-- JSON EXTRACTION

-- | Extract "code" blocks from html and try to parse to a 'Value'
getJsonBlocks :: C8.ByteString -> IO [Maybe Value]
getJsonBlocks doc' = fmap (map extractJson) jsonBlocks
  where doc = parseHtml $ C8.unpack doc'
        jsonBlocks = runX $ doc >>> css "code" //> getText
        extractJson :: String -> Maybe Value
        extractJson = decode' . LC8.pack

-- | Extract internal documentation links from the subnav list
getLinks :: C8.ByteString -> IO [C8.ByteString]
getLinks doc' = fmap (map C8.pack) links
  --doc <- getUrl "/docs" >>= return . parseHtml
  where doc = parseHtml $ C8.unpack doc'
        links = runX $ doc >>> css ".subnav a" ! "href"


-- allJsonBlocks :: IO [BL.ByteString]
-- allJsonBlocks = getLinks >>= mapM getJsonBlocks >>= return . concat

crawl :: IO ()
crawl = do
  links <- getLinks =<< getUrl "/docs"

  forM_ links $ \lnk -> do
     let fname = C8.unpack $ C8.tail lnk
     putStrLn $ "Fetching " ++ fname
     withFileAsOutputExt fname WriteMode NoBuffering $ \outStream -> do
        inStream <- getUrl lnk >>= S.fromByteString
        S.connect inStream outStream


main :: IO ()
main = do
  links <- docLinksLocal "docs"
  byModule <- extractByModule C8.readFile links
  forM_ byModule $ \(mod, valStream) -> do
    print "----"
    print mod
    st <- extractLocalState valStream
    dataDecls <- evalStateT (resolveDependencies st) initParseState
    mapM_ print dataDecls
    print "#####"
  
  where
    -- getLinksFn = docLinks >>= debugInput id "LINKS" S.stdout
    -- getContentFn = getUrl
    getLinksFn = docLinksLocal "docs" >>= S.fromList >>= debugInput C8.pack "LINKS" S.stdout
    getContentFn = C8.readFile


type PageFetch a b = a -> IO b

-- | Extract json 'Values' from a link to a web page.
-- Web page can be local or remode with the appropriate fetchAction
getJson :: PageFetch a C8.ByteString -> InputStream a -> IO (InputStream Value)
getJson fetchAction = S.mapM (fetchAction >=> getJsonBlocks)
                             >=> debugInput lenMaybes "JSON-BLOCKS" S.stdout
                             >=> S.map catMaybes
                             >=> S.concatLists
                             
extractJsonLocal :: IO (InputStream Value)
extractJsonLocal = docLinksLocal "docs" >>= S.fromList >>= getJson C8.readFile

extractByModule :: PageFetch a C8.ByteString
                -> [a]
                -> IO [(a, IO (InputStream Value))]
extractByModule fetch links = do
  forM links $ \l -> do
    inStream <- S.fromList [l]
    return (l, getJson fetch inStream)
    

extractLocalState :: IO (InputStream Value) -> IO (ApiParseState Value)
extractLocalState vals = do
  lst <- vals >>= S.toList
  let st =  mapM parseTopLevel lst
  execStateT st initParseState

-- UTILITIES

-- | Represent a list of @'Maybe' a@ as a string of @(#Just a's / # total)
lenMaybes :: [Maybe a] -> C8.ByteString
lenMaybes ml = C8.pack $ (lenStr $ catMaybes ml) </> (lenStr ml)
  where lenStr = show . length

-- TESTING
testEstimates inVals = do
  st <- extractLocalState inVals
  evalStateT (resolveDependencies st) initParseState
  
justEstimates = (S.fromList ["docs/estimates"] >>= getJson C8.readFile)