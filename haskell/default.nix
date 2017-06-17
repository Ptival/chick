{ nixpkgs ? import <nixpkgs> {}, compiler }:
let bound = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./bound.nix {}; in
let packaged = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chick.nix {
  inherit bound;
}; in
(nixpkgs.pkgs.haskell.lib.addBuildTools
  packaged
  [ nixpkgs.pkgs.cabal-install
    nixpkgs.pkgs.stack
    nixpkgs.pkgs.haskell.packages.${compiler}.intero
  ])
