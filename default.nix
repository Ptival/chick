{ nixpkgs ? import <nixpkgs> {}, compiler }:
let packaged = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chick.nix {
  #base-orphans =
  #  let base-orphans = nixpkgs.pkgs.haskell.packages.${compiler}.base-orphans; in
  #  nixpkgs.pkgs.haskell.lib.dontCheck base-orphans;
}; in
(nixpkgs.pkgs.haskell.lib.addBuildTools
  packaged
  [ nixpkgs.pkgs.cabal-install
    nixpkgs.pkgs.stack
    nixpkgs.pkgs.haskell.packages.${compiler}.intero
  ])
