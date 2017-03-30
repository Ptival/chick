{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.stdenv.mkDerivation {
  name = "chick-coq";
  buildInputs = (with nixpkgs; [
    coq_8_6
  ] ++ (with coqPackages_8_6; [
    coq-ext-lib
  ]));
  shellHook = ''
    export NIXSHELL="$NIXSHELL\[chick\]"
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt"
  '';
}
