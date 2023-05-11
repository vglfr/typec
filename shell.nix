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
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/988cc958c57ce4350ec248d2d53087777f9e1949.tar.gz") { inherit config; }
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
