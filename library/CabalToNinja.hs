-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/CabalToNinja.hs
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : CabalToNinja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module CabalToNinja
  ( module CabalToNinja -- FIXME: specific export list
  ) where

import qualified Language.Ninja.AST                 as AST
import qualified Language.Ninja.Compile             as Compile
import qualified Language.Ninja.Errors              as Errors
import qualified Language.Ninja.IR                  as IR
import qualified Language.Ninja.Lexer               as Lexer
import qualified Language.Ninja.Misc                as Misc
import qualified Language.Ninja.Mock                as Mock
import qualified Language.Ninja.Parser              as Parser
import qualified Language.Ninja.Pretty              as Pretty
import qualified Language.Ninja.Shake               as Shake

import qualified Distribution.Simple.Build          as Cabal
import qualified Distribution.Simple.Configure      as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.PreProcess     as Cabal
import qualified Distribution.Simple.Setup          as Cabal
import qualified Distribution.Verbosity             as Cabal

import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Lens
import           Control.Lens.Setter

import           Control.Monad

import           Flow

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as Text

import           Filesystem.Path.CurrentOS          ((</>))
import qualified Filesystem.Path.CurrentOS          as FP

import           System.Environment                 (getArgs)

--------------------------------------------------------------------------------

-- cabalConfigure :: Misc.Path -> Cabal.ConfigFlags -> IO ()
-- cabalConfigure projectPath configFlags = do
--   let verbosity = C.normal
--   Cabal.readPackageDescription verbosity
--   let gpd = undefined
--   let hbi = undefined
--   lbi <- Cabal.configure (gpd, hbi) configFlags
--   let dist = FP.encodeString ((projectPath ^. Misc.pathFP) </> "dist")
--   Cabal.writePersistBuildConfig dist lbi

cabalPreprocess :: Misc.Path -> IO ()
cabalPreprocess projectPath = do
  let verbosity = Cabal.normal
  let dist = FP.encodeString ((projectPath ^. Misc.pathFP) </> "dist")
  lbi <- Cabal.getPersistBuildConfig dist
  let pkg = Cabal.localPkgDescr lbi
  let components = Cabal.pkgEnabledComponents pkg
  let handlers = Cabal.knownSuffixHandlers
  Cabal.writeAutogenFiles verbosity pkg lbi
  forM_ components
    $ \c -> Cabal.preprocessComponent pkg c lbi False verbosity handlers

generateNinja :: Misc.Path -> IO (AST.Ninja ())
generateNinja projectPath = do
  undefined -- FIXME

--------------------------------------------------------------------------------

tests :: IO ()
tests = pure ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [input, output] <- getArgs
  let toPath str = str ^. from Misc.pathString
  ninja <- generateNinja (toPath input)
  let pretty = Pretty.prettyNinja ninja
  Text.writeFile output pretty

--------------------------------------------------------------------------------
