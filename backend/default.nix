let

  name = "chick";
  compiler-nix-name = "ghc884";

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {
      sourcesOverride = {
        hackageSrc = fetchTarball { inherit (sources."hackage.nix") url sha256; };
      };
  };

  pkgs = import sources.nixpkgs haskellNix.nixpkgsArgs;

  project = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ../.;
      subDir = "backend";
    };

  };

in

project.${name}.components.library // {

  shell = project.shellFor {

    buildInputs =
      let
        hsPkgs = pkgs.haskell.packages.${compiler-nix-name};
      in [
        hsPkgs.haskell-language-server
      ];

    packages = p: [
      p.chick
      p.language-ocaml
    ];

    tools = {
      cabal = "3.2.0.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
