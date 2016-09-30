module Main where

import Options.Applicative
import System.Directory
import System.FilePath
import Text.Regex.Posix
import qualified Data.ByteString as B

data Options = Options
    { rootDir :: FilePath
    , search :: String
    , replace :: String
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

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
    (fullDesc <> progDesc "Search / replace tool for code repositories"
    <> header "Search / replace code")

main :: IO ()
main = do
    opts <- execParser optsParserInfo
    mapDir (search opts) (replace opts) (rootDir opts)

notList :: [String]
notList = [".git", "deps", "doc", "_build", "__pycache__", "node_modules"]

runSearch :: String -> String -> FilePath -> IO ()
runSearch str "" filename = do
    contents <- B.readFile filename
    if contents =~ str
        then putStrLn filename
        else return ()
runSearch str replaceStr filename = putStrLn filename -- search and replace

mapDir :: String -> String -> FilePath -> IO ()
mapDir str replaceStr path = do
    isFile <- doesFileExist path
    if isFile then runSearch str replaceStr path
    else listDirectory path >>=
        mapM_ (mapDir str replaceStr . (path </>)) . filter (`notElem` notList)
