{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = (import nixpkgs) nixpkgsArgs;
  release = import ./release.nix { inherit nixpkgs nixpkgsArgs compiler; };
  drv = release.cabal2ninja;
};

if pkgs.lib.inNixShell then drv.env else drv
