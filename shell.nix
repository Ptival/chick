{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let chick = (import ./default.nix { inherit nixpkgs compiler; }).env; in
nixpkgs.stdenv.lib.overrideDerivation chick (old: {
  buildInputs = (with nixpkgs; [
    cabal-install
  ]);
  shellHook = ''
    export NIXSHELL="$NIXSHELL\[chick\]"
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt"
  '';
})
