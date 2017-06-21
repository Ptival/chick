{ nixpkgs ? import <nixpkgs> {}, compiler }:
let bound = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./bound.nix {}; in
let extensible-effects = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./extensible-effects.nix {}; in
let freer = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./freer.nix {}; in
let packaged = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chick.nix {
  inherit bound;
  inherit extensible-effects;
}; in
(nixpkgs.pkgs.haskell.lib.addBuildTools
  packaged
  [ nixpkgs.pkgs.cabal-install
    nixpkgs.pkgs.stack
    nixpkgs.pkgs.haskell.packages.${compiler}.intero
  ])
