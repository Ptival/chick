{ nixpkgs ? import <nixpkgs> {}
}:
nixpkgs.stdenv.mkDerivation {
  name = "peacoq-frontend";
  buildInputs = (with nixpkgs; [
    ghc         # needed for stack
    nodejs
    purescript
    stack       # needed for npm's purescript
  ]);
  nativeBuildInputs = (with nixpkgs; [
  ]);
  shellHook = ''
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt"
    export PATH=`pwd`/node_modules/.bin:$PATH
  '';
}
