module Storm.Internal.Main
  ( defaultMain
  , mainWith
  )
where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Data.Maybe as Maybe
import qualified Data.Version as Version
import qualified Paths_storm as This
import qualified Storm.Internal.Helper as Helper
import qualified Storm.Internal.Type.Replay as Replay
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as Path
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  config <- getConfig arguments
  Monad.when (configShowHelp config) $ do
    printHelp name
    Exit.exitFailure
  Monad.when (configShowVersion config) $ do
    printVersion
    Exit.exitFailure
  let decode = getDecoder config
  input <- getInput config
  replay <- either fail pure $ decode input
  let encode = getEncoder config
  putOutput config $ encode replay

getConfig :: [String] -> IO Config
getConfig arguments = do
  let
    (updates, unexpectedArguments, unknownOptions, problems) =
      Console.getOpt' Console.Permute options arguments
  mapM_ printUnexpectedArgument unexpectedArguments
  mapM_ printUnknownOption unknownOptions
  mapM_ printProblem problems
  Monad.unless (null problems) Exit.exitFailure
  either fail pure (Monad.foldM applyUpdate defaultConfig updates)

data Config = Config
  { configCompactJson :: Bool
  , configInputFile :: Maybe FilePath
  , configMode :: Maybe Mode
  , configOutputFile :: Maybe FilePath
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  }

options :: [Option]
options =
  [ Console.Option
    ['c']
    ["compact"]
    (Console.NoArg (\config -> pure config { configCompactJson = True }))
    "compact JSON output"
  , Console.Option
    ['h', '?']
    ["help"]
    (Console.NoArg (\config -> pure config { configShowHelp = True }))
    "show the help"
  , Console.Option
    ['i']
    ["input"]
    (Console.ReqArg
      (\input config -> pure config { configInputFile = Just input })
      "FILE"
    )
    "input file"
  , Console.Option
    ['m']
    ["mode"]
    (Console.ReqArg
      (\rawMode config -> do
        mode <- parseMode rawMode
        pure config { configMode = Just mode }
      )
      "MODE"
    )
    "parse or generate"
  , Console.Option
    ['o']
    ["output"]
    (Console.ReqArg
      (\output config -> pure config { configOutputFile = Just output })
      "FILE"
    )
    "output file"
  , Console.Option
    ['v']
    ["version"]
    (Console.NoArg (\config -> pure config { configShowVersion = True }))
    "show the version"
  ]

type Option = Console.OptDescr (Config -> Either String Config)

data Mode
  = ModeGenerate
  | ModeParse

parseMode :: String -> Either String Mode
parseMode x = case x of
  "generate" -> Right ModeGenerate
  "parse" -> Right ModeParse
  _ -> Left $ "invalid mode: " <> show x

printUnexpectedArgument :: String -> IO ()
printUnexpectedArgument argument =
  IO.hPutStrLn IO.stderr $ "WARNING: unexpected argument `" <> argument <> "'"

printUnknownOption :: String -> IO ()
printUnknownOption option =
  IO.hPutStrLn IO.stderr $ "WARNING: unknown option `" <> option <> "'"

printProblem :: String -> IO ()
printProblem problem = IO.hPutStrLn IO.stderr $ "ERROR: " <> problem

applyUpdate
  :: Config -> (Config -> Either String Config) -> Either String Config
applyUpdate config update = update config

defaultConfig :: Config
defaultConfig = Config
  { configCompactJson = False
  , configInputFile = Nothing
  , configMode = Nothing
  , configOutputFile = Nothing
  , configShowHelp = False
  , configShowVersion = False
  }

printHelp :: String -> IO ()
printHelp name = IO.hPutStrLn IO.stderr
  $ Console.usageInfo (name <> "-" <> versionString) options

versionString :: String
versionString = Version.showVersion This.version

printVersion :: IO ()
printVersion = IO.hPutStrLn IO.stderr versionString

getDecoder :: Config -> Bytes.ByteString -> Either String Replay.Replay
getDecoder config bytes = case getMode config of
  ModeGenerate -> Aeson.eitherDecodeStrict bytes
  ModeParse -> Helper.parseReplay bytes

getMode :: Config -> Mode
getMode config =
  Maybe.fromMaybe
      (case Path.takeExtension <$> configInputFile config of
        Just ".json" -> ModeGenerate
        Just ".replay" -> ModeParse
        _ -> case Path.takeExtension <$> configOutputFile config of
          Just ".json" -> ModeParse
          Just ".replay" -> ModeGenerate
          _ -> ModeParse
      )
    $ configMode config

getInput :: Config -> IO Bytes.ByteString
getInput = maybe Bytes.getContents Bytes.readFile . configInputFile

getEncoder :: Config -> Replay.Replay -> Builder.Builder
getEncoder config replay = case getMode config of
  ModeGenerate -> Helper.generateReplay replay
  ModeParse ->
    Builder.lazyByteString
      $ (if configCompactJson config
          then Aeson.encode
          else Aeson.encodePretty' Aeson.defConfig
            { Aeson.confIndent = Aeson.Spaces 2
            , Aeson.confCompare = compare
            , Aeson.confTrailingNewline = True
            }
        )
          replay

putOutput :: Config -> Builder.Builder -> IO ()
putOutput config builder = case configOutputFile config of
  Nothing -> Builder.hPutBuilder IO.stdout builder
  Just file ->
    IO.withBinaryFile file IO.WriteMode (`Builder.hPutBuilder` builder)
