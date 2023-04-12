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
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/53dad94e874c9586e71decf82d972dfb640ef044.tar.gz") { inherit config; }
  }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.cabal-install
      pkgs.haskell.compiler.ghc943
      pkgs.haskell-language-server
      pkgs.haskellPackages.hoogle
      pkgs.nasm
    ];
  }
