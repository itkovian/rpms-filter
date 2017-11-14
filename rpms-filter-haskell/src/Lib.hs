{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( regexify
    , removals
    ) where

import qualified Data.Text              as T
import           Text.Regex.PCRE
import           Text.Regex.PCRE.Text
import           System.FilePath.Posix  (takeBaseName)

-- | `regixify` transforms a string with shell-style wildcards into a regular expression
regexify :: T.Text -> (T.Text, T.Text)
regexify packagePattern =
    (T.takeWhile (/= '*') packagePattern, T.replace "*" "[-a-zA-Z0-9_.]*" packagePattern)


-- | `removals` removes the packages that do not match a regular expression from a list, retaining those that do not match
removals :: [(T.Text, T.Text)]  -- ^ list with (prefix, regular expression) for the locked packages
         -> [T.Text]            -- ^ list with the removal candidates (absolute path)
         -> [T.Text]            -- ^ packages to remove
removals [] ps = ps
removals _ [] = []
removals rss@((prefix, regex):rs) ps@(p:pkgs) =
    let bp = T.pack $ takeBaseName $ T.unpack p
    in if T.unpack p =~ T.unpack regex
        then removals rss pkgs
        else case compare bp prefix of
                GT -> removals rs ps
                _  -> p : removals rss pkgs
