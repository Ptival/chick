# { nur ? (import <nixpkgs> {}).nur.repos.ptival
{ nur ? (import ~/personal/nur-packages {})
, nixpkgs ? import <nixpkgs> { overlays = [ nur.overlays.chick ]; }
}:
with nixpkgs;
haskell.lib.doCheck (haskellPackages.callCabal2nix "chick" ./. {})
