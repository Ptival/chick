{ nur ? (import ~/personal/nur-packages {})
, nixpkgs ? import <nixpkgs> { overlays = [ nur.overlays.chick ]; }
}:
with nixpkgs;
mkShell {
  buildInputs = [
    cabal-install
    haskellPackages.hlint
  ];
  inputsFrom = [
    (haskellPackages.callCabal2nix "chick" ./. {}).env
  ];
  name = "chick";
}
