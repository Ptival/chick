let

  name = "chick";
  compiler-nix-name = "ghc883";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = import (fetchNiv "hackage.nix");
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
    ];
  });

  hls-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.fetchFromGitHub {
      name = "haskell-language-server";
      inherit (sources.haskell-language-server) owner repo rev;
      # owner = "haskell";
      # repo = "haskell-language-server";
      # rev = "0.5.0";
      # Need to override the hash due to lack of niv submodule support
      sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
      fetchSubmodules = true;
    };
    # src = fetchNiv "haskell-language-server";
    lookupSha256 = { location, tag, ... } : {
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    }."${location}"."${tag}";
    inherit compiler-nix-name; # index-state; # checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "0ga7a2d22hbxvhx4363g0iyss4x5kaamkxh7bhmcki6azsha92vz";
    modules = [{
      # Tests don't pass for some reason, but this is a somewhat random revision.
      packages.haskell-language-server.doCheck = false;
    }];
  };

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    modules = [

      # {
      #   reinstallableLibGhc = true;
      # }

      {
        # Make Cabal reinstallable
        nonReinstallablePkgs = [

          "array"
          "base"
          "binary"
          "bytestring"
          # "Cabal"
          "containers"
          "deepseq"
          "directory"
          "filepath"
          "ghc"
          "ghc-prim"
          "hpc"
          "integer-gmp"
          "mtl"
          "parsec"
          "pretty"
          "process"
          "rts"
          "template-haskell"
          "terminfo"
          "text"
          "time"
          "transformers"
          "unix"

        ];
      }

    ];

    pkg-def-extras = [
      (hackage: {
        packages = {
          fourmolu = hackage.fourmolu."0.1.0.0".revisions.default;
          shake = hackage.shake."0.18.4".revisions.default;
        };
      })
    ];

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

set.${name}.components.library // {

  shell = set.shellFor {

    buildInputs =
      [
        hls-set.haskell-language-server.components.exes.haskell-language-server
        set.happy.components.exes.happy
      ];

    exactDeps = false;

    packages = p: [
      p.${name}
      p.language-ocaml
    ];

    tools = {
      cabal = {
        inherit compiler-nix-name;
        version = "3.2.0.0";
      };
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
