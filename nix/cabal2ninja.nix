{ mkDerivation, aeson, aeson-diff, aeson-pretty, base, bytestring
, concurrent-supply, containers, cryptonite, deepseq, directory
, Earley, exceptions, extra, flow, hashable, HUnit, intern
, language-ninja, lens, makefile, megaparsec, monad-mock, mtl
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, reflection, shake, smallcheck
, smallcheck-lens, stdenv, system-filepath, tasty, tasty-golden
, tasty-html, tasty-hunit, tasty-lens, tasty-quickcheck
, tasty-smallcheck, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "cabal2ninja";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-diff aeson-pretty base bytestring concurrent-supply
    containers cryptonite deepseq directory Earley exceptions extra
    flow hashable HUnit intern language-ninja lens makefile megaparsec
    monad-mock mtl prettyprinter prettyprinter-ansi-terminal QuickCheck
    quickcheck-instances reflection shake smallcheck smallcheck-lens
    system-filepath tasty tasty-golden tasty-html tasty-hunit
    tasty-lens tasty-quickcheck tasty-smallcheck text transformers
    turtle unordered-containers versions
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/awakesecurity/cabal2ninja";
  description = "A tool for building Cabal projects with Ninja";
  license = stdenv.lib.licenses.asl20;
}
