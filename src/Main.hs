{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default (def)
import           Data.Monoid ((<>))
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

renderPage :: FrontitConf -> Gitit.Page -> String
renderPage conf pg@Gitit.Page { .. } = Pandoc.writeHtmlString writerOpts pandoc
  where
    writerOpts = def { Pandoc.writerTemplate   = fcTemplate conf
                     , Pandoc.writerStandalone = True
                     }
    rawPage = Gitit.pageToString (fcGititConfig conf) pg
    pandoc = Pandoc.handleError (reader def rawPage)
    reader =
      case pageFormat of
        Gitit.Markdown   -> Pandoc.readMarkdown
        Gitit.CommonMark -> Pandoc.readCommonMark
        Gitit.RST        -> Pandoc.readRST
        Gitit.LaTeX      -> Pandoc.readLaTeX
        Gitit.HTML       -> Pandoc.readHtml
        Gitit.Textile    -> Pandoc.readTextile
        Gitit.Org        -> Pandoc.readOrg
        Gitit.DocBook    -> Pandoc.readDocBook
        Gitit.MediaWiki  -> Pandoc.readMediaWiki

getLocalPath :: FrontitConf -> BS.ByteString -> Maybe FilePath
getLocalPath conf req
  | BS.any (== '.') req = Nothing
  | req == "/" = Just (Gitit.frontPage (fcGititConfig conf) <> ".page")
  | otherwise = Just (BS.unpack (req <> ".page"))

app :: FrontitConf -> Wai.Application
app conf = \ req respond -> do
  let respond' st pg = respond (Wai.responseLBS st [] (LBS.pack pg))
  case getLocalPath conf (Wai.rawPathInfo req) of
    Nothing -> respond' Http.status403 "invalid URL"
    Just path -> do
      putStrLn ("fetching: " <> path)
      result <- fetchPage conf path
      case result of
        Found pg -> respond' Http.status200 (renderPage conf pg)
        Private  -> respond' Http.status403 "private page"
        NotFound -> respond' Http.status404 "not found"

main :: IO ()
main = do
  args <- Env.getArgs
  opts <- case Opt.getOpt Opt.Permute frontitOptDescr args of
    (fs, [], []) -> return (foldr (.) id fs defaultOpts)
    _            -> do
      putStrLn (Opt.usageInfo "frontit" frontitOptDescr)
      Exit.exitFailure
  conf <- optsToConfiguration opts
  Warp.run (optPort opts) (app conf)
