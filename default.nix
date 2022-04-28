{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cabal-install
    ghc
    haskell-language-server
  ];
}

