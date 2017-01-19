{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default (def)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Gitit.Page as Gitit
import qualified Network.Gitit.Types as Gitit
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir
import           System.FilePath ((</>), makeRelative)
import qualified Text.Pandoc as Pandoc
import           Text.Printf (printf)

import           Config

data Result
  = Found Gitit.Page
  | Private
  | NotFound

fetchPage :: FrontitConf -> FilePath -> IO Result
fetchPage conf path = do
  let localPath = fcData conf </> (makeRelative "/" path)
  exists <- Dir.doesFileExist localPath
  if not exists
    then return NotFound
    else do
      rawPage <- readFile localPath
      let pg = Gitit.stringToPage (fcGititConfig conf) path rawPage
      case lookup "public" (Gitit.pageMeta pg) of
        Just "yes" | fcProtection conf == PrivateByDefault -> return (Found pg)
        Just "no"  | fcProtection conf == PublicByDefault -> return Private
        _ | fcProtection conf == PublicByDefault -> return (Found pg)
          | otherwise -> return Private

convertLinks :: Pandoc.Inline -> Pandoc.Inline
convertLinks link@(Pandoc.Link as name ("",title)) =
  case flatten name of
    Nothing -> link
    Just text -> Pandoc.Link as name (text, title)
convertLinks rs = rs

flatten :: [Pandoc.Inline] -> Maybe String
flatten = fmap mconcat . sequence . fmap go
  where go (Pandoc.Str s) = Just s
        go Pandoc.Space   = Just " "
        go _              = Nothing

addTitle :: T.Text -> Pandoc.Pandoc -> Pandoc.Pandoc
addTitle title p@(Pandoc.Pandoc meta@(Pandoc.Meta metaMap) bs)
  | Just _ <- Pandoc.lookupMeta "title" meta = p
  | otherwise = Pandoc.Pandoc newMeta bs
      where titleVal = Pandoc.MetaString (T.unpack title)
            newMeta  = Pandoc.Meta (Map.insert "title" titleVal metaMap)

renderPage :: FrontitConf -> Gitit.Page -> T.Text -> String
renderPage conf pg@Gitit.Page { .. } title =
  Pandoc.writeHtmlString writerOpts (addTitle title pandoc)
  where
    writerOpts = def { Pandoc.writerTemplate   = fcTemplate conf
                     , Pandoc.writerStandalone = True
                     }
    rawPage = Gitit.pageToString (fcGititConfig conf) pg
    pandoc = Pandoc.bottomUp convertLinks (Pandoc.handleError parsed)
    parsed =
      case pageFormat of
        Gitit.Markdown   -> Pandoc.readMarkdown def rawPage
        Gitit.CommonMark -> Pandoc.readCommonMark def rawPage
        Gitit.RST        -> Pandoc.readRST def pageText
        Gitit.LaTeX      -> Pandoc.readLaTeX def pageText
        Gitit.HTML       -> Pandoc.readHtml def pageText
        Gitit.Textile    -> Pandoc.readTextile def pageText
        Gitit.Org        -> Pandoc.readOrg def pageText
        Gitit.DocBook    -> Pandoc.readDocBook def pageText
        Gitit.MediaWiki  -> Pandoc.readMediaWiki def pageText

getLocalPath :: FrontitConf -> T.Text -> Maybe FilePath
getLocalPath conf req
  | T.any (== '.') req = Nothing
  | req == "" = Just (Gitit.frontPage (fcGititConfig conf) <> ".page")
  | otherwise = Just (T.unpack (req <> ".page"))

getTitle :: FrontitConf -> [T.Text] -> T.Text
getTitle conf [] = T.pack (Gitit.frontPage (fcGititConfig conf))
getTitle _ cs = T.toTitle (last cs)

strToByteString :: String -> LBS.ByteString
strToByteString = LBS.fromStrict . T.encodeUtf8 . T.pack

-- Our application is simple: every GET request will look up a
-- corresponding file in the data directory and serve it according
-- to roughly the same logic as Gitit.
app :: FrontitConf -> Wai.Application
app conf = \ req respond -> do
  let respond' st pg = respond (Wai.responseLBS st [] (strToByteString pg))
  if Wai.requestMethod req == "GET"
    then do
      case getLocalPath conf (T.intercalate ("/") (Wai.pathInfo req)) of
        Nothing -> respond' Http.status403 "invalid URL"
        Just path -> do
          let title = getTitle conf (Wai.pathInfo req)
          result <- fetchPage conf path
          case result of
            Found pg -> respond' Http.status200 (renderPage conf pg title)
            Private  -> respond' Http.status403 "private page"
            NotFound -> respond' Http.status404 "not found"
    else respond' Http.status403 "forbidden"

-- Something like this exists in wai-extra, but I don't want to have
-- to depend on an even bigger set of deps after gitit+pandoc, so
-- this is a quick reimplementation.
mkLogger :: Wai.Application -> Wai.Application
mkLogger app' = \ req respond -> app' req $ \ resp -> do
  currentTime <- Time.getCurrentTime
  let time = Time.formatTime
               Time.defaultTimeLocale
               "[%d/%b/%Y:%H:%M:%S %z]"
               currentTime
  printf "%v - - %v \"%v %v %v\" %v -\n"
    (show (Wai.remoteHost req))
    time
    (BS.unpack (Wai.requestMethod req))
    (BS.unpack (Wai.rawPathInfo req))
    (show (Wai.httpVersion req))
    (Http.statusCode (Wai.responseStatus resp))
  respond resp

main :: IO ()
main = do
  conf <- getConfig
  printf "running frontit on port %d\n" (fcPort conf)
  Warp.run (fcPort conf) (mkLogger (app conf))
