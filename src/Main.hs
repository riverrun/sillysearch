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
    , extension :: [String]
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
    <*> (many . strOption)
        (long "extension" <> short 'x'
        <> metavar "EXTENSION" <> help "File extensions to search")

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
    (fullDesc <> progDesc "Silly tool to search / replace code"
    <> header "Search / replace tool for code repositories")

main :: IO ()
main = do
    opts <- execParser optsParserInfo
    if replace opts == ""
        then putStrLn $ "Searching for " ++ search opts ++ " in " ++ rootDir opts
        else putStrLn $ "Replacing " ++ search opts ++ " with " ++ replace opts ++ " in " ++ rootDir opts
    mapDir opts (rootDir opts)

notList :: [String]
notList = [".git", "deps", "doc", "_build", "__pycache__", "node_modules", ".stack-work", "tmp"]

mapDir :: Options -> FilePath -> IO ()
mapDir opts path = do
    isFile <- doesFileExist path
    if isFile
        then runTask opts path
        else listDirectory path >>=
            mapM_ (mapDir opts . (path </>)) . filter (`notElem` notList)

runTask :: Options -> FilePath -> IO ()
runTask opts@Options {extension = ext} filename =
    when (any (\x -> ("." ++ x) `isSuffixOf` filename) ext || null ext) $
    BC.readFile filename >>= searchReplace opts filename

searchReplace :: Options -> FilePath -> BC.ByteString -> IO ()
searchReplace Options {search = str, replace = ""} filename contents =
    when (contents =~ str) $ putStrLn $ "\t" ++ filename
searchReplace Options {search = str, replace = rep} filename contents =
    when (contents =~ str) $ do
    putStrLn $ "\t" ++ filename
    newContents <- substituteCompile (BC.pack str) contents (BC.pack rep)
    case newContents of
        Left err -> putStrLn err
        Right newContents -> BC.writeFile filename newContents
