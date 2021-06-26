{ ghcVersion }:
let pkgs = import <nixpkgs> {};
in with pkgs;
  mkShell {
    buildInputs = [
      pkgs.haskell.compiler."ghc${ghcVersion}"
      cabal-install
      # DB Deps
      postgresql_13
      gmp
      zlib
      glibcLocales
    ];
  }
