{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "ai-challenger";

  buildInputs = [ haskellPackages.cabal-install haskell.compiler.ghc7101 zlib ];

  shellHook = ''
    export LD_LIBRARY_PATH="${zlib}/lib:$LD_LIBRARY_PATH"
  '';
}