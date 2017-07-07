{ mkDerivation, aeson, aeson-diff, aeson-pretty, base, bytestring
, concurrent-supply, containers, cryptonite, deepseq, directory
, Earley, exceptions, extra, fetchgit, flow, hashable, HUnit
, intern, lens, makefile, megaparsec, monad-mock, mtl
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, reflection, shake, smallcheck
, smallcheck-lens, stdenv, system-filepath, tasty, tasty-golden
, tasty-html, tasty-hunit, tasty-lens, tasty-quickcheck
, tasty-smallcheck, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/awakesecurity/language-ninja";
    sha256 = "0pf548dqsd68hwfdfzn7pywbnh99ifsgml64inp4xqldm4igb480";
    rev = "76966e034355c5bc45075d98b9325c705a873d18";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers deepseq directory
    Earley exceptions extra flow hashable intern lens makefile
    megaparsec mtl prettyprinter prettyprinter-ansi-terminal QuickCheck
    quickcheck-instances reflection shake smallcheck system-filepath
    text transformers unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring concurrent-supply containers
    cryptonite flow hashable lens makefile mtl prettyprinter
    prettyprinter-ansi-terminal text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-diff aeson-pretty base bytestring containers flow
    hashable HUnit lens monad-mock mtl QuickCheck quickcheck-instances
    reflection smallcheck smallcheck-lens system-filepath tasty
    tasty-golden tasty-html tasty-hunit tasty-lens tasty-quickcheck
    tasty-smallcheck text transformers turtle unordered-containers
    versions
  ];
  homepage = "https://github.com/awakesecurity/language-ninja";
  description = "A Haskell library for parsing, pretty-printing, and compiling the Ninja build language";
  license = stdenv.lib.licenses.asl20;
}
