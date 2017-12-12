{ nixpkgs ? import <nixpkgs> {}, ghc }:
nixpkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "chick";
  buildInputs = (with nixpkgs; [
    binutils
    git
    gmp
    ncurses
    #stack
    zlib
  ]);
  shellHook = ''
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt"
  '';
}
