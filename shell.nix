{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let chick = (import ./default.nix { inherit nixpkgs compiler; }).env; in
nixpkgs.stdenv.lib.overrideDerivation chick (old: {
  buildInputs = (with nixpkgs; [
    cabal-install
    coq_8_6
  ]);
  shellHook = ''
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt"
  '';
})
