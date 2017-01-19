{-# LANGUAGE RecordWildCards #-}

module Config where

import qualified Network.Gitit.Config as Gitit
import qualified Network.Gitit.Types as Gitit
import qualified System.Console.GetOpt as Opt
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit

data Protection
  = PrivateByDefault
  | PublicByDefault
    deriving (Eq, Show)

data FrontitOpts = FrontitOpts
  { optPort        :: Int
  , optData        :: Maybe FilePath
  , optGititConfig :: Maybe FilePath
  , optTemplate    :: Maybe FilePath
  , optProtection  :: Protection
  } deriving (Eq, Show)

defaultOpts :: FrontitOpts
defaultOpts = FrontitOpts
  { optPort        = 5000
  , optData        = Nothing
  , optGititConfig = Nothing
  , optTemplate    = Nothing
  , optProtection  = PrivateByDefault
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
  , Opt.Option ['o'] ["public-by-default"]
      (Opt.NoArg (\ opts -> opts { optProtection = PublicByDefault}))
      "Allow all pages to be browsed by default"
  ]

data FrontitConf = FrontitConf
  { fcData        :: FilePath
  , fcGititConfig :: Gitit.Config
  , fcTemplate    :: String
  , fcProtection  :: Protection
  , fcPort        :: Int
  }

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
  let fcPort = optPort
      fcProtection = optProtection
  return FrontitConf { .. }

defaultTemplate :: String
defaultTemplate = "<html><body>$body$</body></html>"

getConfig :: IO FrontitConf
getConfig = do
  args <- Env.getArgs
  opts <- case Opt.getOpt Opt.Permute frontitOptDescr args of
    (fs, [], []) -> return (foldr (.) id fs defaultOpts)
    _            -> do
      putStrLn (Opt.usageInfo "frontit" frontitOptDescr)
      Exit.exitFailure
  optsToConfiguration opts
