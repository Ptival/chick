{ nixpkgs ? import <nixpkgs> {}, compiler }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chick.nix {
  #base-orphans =
  #  let base-orphans = nixpkgs.pkgs.haskell.packages.${compiler}.base-orphans; in
  #  nixpkgs.pkgs.haskell.lib.dontCheck base-orphans;
}
