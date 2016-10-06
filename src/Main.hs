module Main where

import Control.Monad
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath
import Text.Regex.Posix
import qualified Data.ByteString as B

data Options = Options
    { rootDir :: FilePath
    , search :: String
    , replace :: String
    , extension :: String
    }

optsParser :: Parser Options
optsParser = Options
    <$> strOption
        (long "directory" <> short 'd' <> value "."
        <> metavar "DIRECTORY" <> help "Root directory")
    <*> argument str
        (metavar "SEARCH" <> help "Search string")
    <*> strOption
        (long "replace" <> short 'r' <> value ""
        <> metavar "REPLACE" <> help "Replace string")
    <*> strOption
        (long "extension" <> short 'x' <> value ""
        <> metavar "EXTENSION" <> help "File extension")

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
    (fullDesc <> progDesc "Search / replace tool for code repositories"
    <> header "Search / replace code")

main :: IO ()
main = do
    opts <- execParser optsParserInfo
    mapDir opts (rootDir opts)

notList :: [String]
notList = [".git", "deps", "doc", "_build", "__pycache__", "node_modules"]

runTask :: Options -> FilePath -> IO ()
runTask opts@Options {extension = ""} filename = runSearch opts filename
runTask opts filename =
    when (extension opts `isSuffixOf` filename) $ runSearch opts filename

runSearch :: Options -> FilePath -> IO ()
runSearch Options {search = str, replace = ""} filename = do
    contents <- B.readFile filename
    when (contents =~ str) $ putStrLn filename
runSearch opts filename = putStrLn filename -- search and replace

mapDir :: Options -> FilePath -> IO ()
mapDir opts path = do
    isFile <- doesFileExist path
    if isFile
        then runTask opts path
        else listDirectory path >>=
            mapM_ (mapDir opts . (path </>)) . filter (`notElem` notList)
