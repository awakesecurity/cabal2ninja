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

{-# LANGUAGE RecordWildCards #-}

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

import qualified Language.Ninja.AST     as AST
import qualified Language.Ninja.Compile as Compile
import qualified Language.Ninja.Errors  as Errors
import qualified Language.Ninja.IR      as IR
import qualified Language.Ninja.Lexer   as Lexer
import qualified Language.Ninja.Misc    as Misc
import qualified Language.Ninja.Mock    as Mock
import qualified Language.Ninja.Parser  as Parser
import qualified Language.Ninja.Pretty  as Pretty
import qualified Language.Ninja.Shake   as Shake

import           Control.Lens

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

import           System.Environment     (getArgs)

generateNinja :: Misc.Path -> IO (AST.Ninja ())
generateNinja projectDir = do
  undefined -- FIXME

tests :: IO ()
tests = pure ()

main :: IO ()
main = do
  [input, output] <- getArgs
  let toPath str = str ^. from Misc.pathString
  ninja <- generateNinja (toPath input)
  let pretty = Pretty.prettyNinja ninja
  Text.writeFile output pretty
