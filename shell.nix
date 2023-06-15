let
  config = {
    packageOverrides = pkgs: {
      haskell-language-server = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "943" ];
      };
    };
  };

in
  {
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5e871d8aa6f57cc8e0dc087d1c5013f6e212b4ce.tar.gz") { inherit config; }
  }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.cabal-install
      pkgs.gcc
      pkgs.haskell.compiler.ghc943
      pkgs.haskell-language-server
      pkgs.haskellPackages.hoogle
      pkgs.nasm
    ];
  }
