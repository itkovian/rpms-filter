module Main where

import Lib

import           Control.Applicative          ((<$>), (<*>))
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import           Data.Text.IO                 (hGetContents, putStr)
import qualified Options.Applicative          as OA
import           System.IO                    hiding (hGetContents, putStr)
import           Prelude                      hiding (hGetContents, putStr)

data Options = Options
    { oLocked :: !FilePath
    , oRemove :: !FilePath
    } deriving (Show)

--------------------------------------------------------------------------------
parserOptions :: OA.Parser Main.Options
parserOptions = Options
    <$> OA.strOption (
            OA.long "locked" <>
            OA.short 'l' <>
            OA.help "File with list of locked packages")
    <*> OA.strOption (
            OA.long "removals" <>
            OA.short 'r' <>
            OA.help "File with removal candicates")


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo Main.Options
parserInfo = OA.info (OA.helper <*> parserOptions)
    (OA.fullDesc
        <> OA.progDesc "Check removal candidates for locked packages"
        <> OA.header "rpms-filter"
    )


main :: IO ()
main = do
    options <- OA.execParser parserInfo

    locked_lines <- (map (T.takeWhile (/= ',')) . T.lines) `fmap` withFile (oLocked options) ReadMode hGetContents
    removal_lines <- T.lines `fmap` withFile (oRemove options) ReadMode hGetContents

    let patterns = map regexify locked_lines

    putStr $ T.unlines $ removals patterns removal_lines
