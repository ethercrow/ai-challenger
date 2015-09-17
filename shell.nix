{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "ai-challenger";

  buildInputs = [ haskellPackages.stack haskell.compiler.ghc7102 zlib nodejs ];

  shellHook = ''
    export LD_LIBRARY_PATH="${zlib}/lib:$LD_LIBRARY_PATH"
  '';
}