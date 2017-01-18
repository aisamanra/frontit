{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default (def)
import           Data.Monoid ((<>))
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Network.Gitit.Config as Gitit
import qualified Network.Gitit.Page as Gitit
import qualified Network.Gitit.Types as Gitit
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Console.GetOpt as Opt
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import           System.FilePath ((</>), makeRelative)
import qualified Text.Pandoc as Pandoc
import           Text.Printf (printf)

data FrontitOpts = FrontitOpts
  { optPort        :: Int
  , optData        :: Maybe FilePath
  , optGititConfig :: Maybe FilePath
  , optTemplate    :: Maybe FilePath
  } deriving (Eq, Show)

defaultOpts :: FrontitOpts
defaultOpts = FrontitOpts
  { optPort        = 5000
  , optData        = Nothing
  , optGititConfig = Nothing
  , optTemplate    = Nothing
  }

frontitOptDescr :: [Opt.OptDescr (FrontitOpts -> FrontitOpts)]
frontitOptDescr =
  [ Opt.Option ['p'] ["port"]
      (Opt.ReqArg (\ s opts -> opts { optPort = read s }) "port")
      "The port to serve on"
  , Opt.Option ['d'] ["data"]
      (Opt.ReqArg (\ s opts -> opts { optData = Just s }) "path")
      "The location of the data directory"
  , Opt.Option ['c'] ["config"]
      (Opt.ReqArg (\ s opts -> opts { optGititConfig = Just s }) "path")
      "The location of the gitit configuration"
  , Opt.Option ['t'] ["template"]
      (Opt.ReqArg (\ s opts -> opts { optTemplate = Just s }) "path")
      "The location of the desired HTML template"
  ]

optsToConfiguration :: FrontitOpts -> IO FrontitConf
optsToConfiguration FrontitOpts { .. } = do
  fcData <- case optData of
    Just path -> return path
    Nothing   -> Dir.getCurrentDirectory
  fcTemplate <- case optTemplate of
    Just path -> readFile path
    Nothing   -> return defaultTemplate
  fcGititConfig <- case optGititConfig of
    Just path -> Gitit.getConfigFromFile path
    Nothing   -> Gitit.getDefaultConfig
  return FrontitConf { .. }

defaultTemplate :: String
defaultTemplate = "<html><body>$body$</body></html>"

data FrontitConf = FrontitConf
  { fcData        :: FilePath
  , fcGititConfig :: Gitit.Config
  , fcTemplate    :: String
  }

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
        Just "yes" -> return (Found pg)
        _          -> return Private

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

renderPage :: FrontitConf -> Gitit.Page -> String
renderPage conf pg@Gitit.Page { .. } = Pandoc.writeHtmlString writerOpts pandoc
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

getLocalPath :: FrontitConf -> BS.ByteString -> Maybe FilePath
getLocalPath conf req
  | BS.any (== '.') req = Nothing
  | req == "/" = Just (Gitit.frontPage (fcGititConfig conf) <> ".page")
  | otherwise = Just (BS.unpack (req <> ".page"))

app :: FrontitConf -> Wai.Application
app conf = \ req respond -> do
  let respond' st pg = respond (Wai.responseLBS st [] (LBS.pack pg))
  if Wai.requestMethod req == "GET"
    then do
      case getLocalPath conf (Wai.rawPathInfo req) of
        Nothing -> respond' Http.status403 "invalid URL"
        Just path -> do
          result <- fetchPage conf path
          case result of
            Found pg -> respond' Http.status200 (renderPage conf pg)
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
  args <- Env.getArgs
  opts <- case Opt.getOpt Opt.Permute frontitOptDescr args of
    (fs, [], []) -> return (foldr (.) id fs defaultOpts)
    _            -> do
      putStrLn (Opt.usageInfo "frontit" frontitOptDescr)
      Exit.exitFailure
  conf <- optsToConfiguration opts
  printf "running frontit on port %d\n" (optPort opts)
  Warp.run (optPort opts) (mkLogger (app conf))
