module Main where

import Control.Monad
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Utils
import qualified Data.ByteString.Char8 as BC

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
notList = [".git", "deps", "doc", "_build", "__pycache__", "node_modules", ".stack-work"]

runTask :: Options -> FilePath -> IO ()
runTask opts@Options {extension = ext} filename =
    when (ext `isSuffixOf` filename || ext == "") $ BC.readFile filename >>=
        searchReplace opts filename

searchReplace :: Options -> FilePath -> BC.ByteString -> IO ()
searchReplace Options {search = str, replace = ""} filename contents =
    when (contents =~ str) $ putStrLn filename
searchReplace Options {search = str, replace = rep} filename contents =
    putStrLn filename -- search and replace

mapDir :: Options -> FilePath -> IO ()
mapDir opts path = do
    isFile <- doesFileExist path
    if isFile
        then runTask opts path
        else listDirectory path >>=
            mapM_ (mapDir opts . (path </>)) . filter (`notElem` notList)
