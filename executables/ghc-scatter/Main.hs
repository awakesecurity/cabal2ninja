-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/ghc-scatter/Main.hs
--
-- License:
--     Copyright 2017 Awake Security
--
--     Licensed under the Apache License, Version 2.0 (the "License");
--     you may not use this file except in compliance with the License.
--     You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--     Unless required by applicable law or agreed to in writing, software
--     distributed under the License is distributed on an "AS IS" BASIS,
--     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--     See the License for the specific language governing permissions and
--     limitations under the License.

{-# LANGUAGE OverloadedStrings #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
module Main (main) where

import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Monoid

import qualified Data.Makefile             as M
import qualified Data.Makefile.Parse       as M
import qualified Data.Makefile.Render      as M

import           Data.Hashable             (Hashable)

import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text

import           Flow

import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (..))

import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FP

import qualified Turtle

tshow :: (Show a) => a -> Text
tshow = show .> Text.pack

spacer :: Int -> Text
spacer n = Text.replicate n "─" <> "\n"

runGHC :: [Text] -> IO ()
runGHC args = do
  let width = 80
  (exitCode, out, err) <- Turtle.procStrictWithErr "ghc" args (pure mempty)
  case exitCode of
    ExitFailure code -> [ "\n\ESC[1mghc exited with ", tshow code, "\ESC[0m\n"
                        , spacer width
                        , "\ESC[1mstdout output:\ESC[0m\n", out
                        , spacer width
                        , "\ESC[1mstderr output:\ESC[0m\n", err
                        , spacer width
                        ] |> mconcat |> Text.unpack |> fail
    ExitSuccess      -> pure ()

fromTarget :: M.Target -> Text
fromTarget (M.Target t) = t

fromDependency :: M.Dependency -> Text
fromDependency (M.Dependency t) = t

toPair :: M.Entry -> (Text, [Text])
toPair (M.Rule tgt deps []) = (fromTarget tgt, map fromDependency deps)
toPair (M.Rule       _ _ _) = error "rule with command found"
toPair (M.Assignment _ _ _) = error "assignment detected"

unionsWith :: (Eq k, Hashable k)
           => (v -> v -> v) -> [HM.HashMap k v] -> HM.HashMap k v
unionsWith merge = foldr (HM.unionWith merge) HM.empty

simplifyMakefile :: M.Makefile -> [(Text, [Text])]
simplifyMakefile = M.entries
                   .> map (toPair .> uncurry HM.singleton)
                   .> unionsWith (<>)
                   .> fmap (HS.fromList .> HS.toList)
                   .> HM.toList

processPair :: Text -> [Text] -> IO ()
processPair target deps = do
  let str = target <> " : " <> Text.intercalate " " deps <> "\n"
  case Text.stripSuffix ".o" target of
    Just file -> Text.writeFile (Text.unpack (file <> ".d")) str
    Nothing   -> fail "did not end in .o"

ghcDeps :: [Text] -> IO M.Makefile
ghcDeps args = do
  tmproot <- Turtle.need "TMPDIR"
             |> fmap (fromMaybe "/tmp" .> Text.unpack .> FP.decodeString)
  Turtle.with (Turtle.mktempdir tmproot "scatter") $ \tmpdir -> do
    let m = FP.encodeString (tmpdir </> "Makefile")
    runGHC (["-dep-suffix", "", "-dep-makefile", Text.pack m, "-M"] <> args)
    M.parseAsMakefile m >>= either fail pure

main :: IO ()
main = do
  args <- map Text.pack <$> getArgs
  ghcDeps args
    >>= simplifyMakefile
    .>  mapM_ (uncurry processPair)
