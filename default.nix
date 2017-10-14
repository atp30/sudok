# require a package set and a haskell package set
{pkgs ? import <nixpkgs> {},
 haskellPackages ? pkgs.haskell.packages.ghc821,
 cabal2nixargs ? "",
 src ? pkgs.lib.cleanSource ./.
 }:

 # read a cabal file
let
     cabal_nix_output = pkgs.stdenv.mkDerivation {
        name = "cabal_nix_output";
        builder = pkgs.writeText "build.sh" "$cabal2nix/bin/cabal2nix $cabal2nixargs $src > $out";
        inherit cabal2nixargs src;
        # src = pkgs.lib.sourceFilesBySuffices ./. [".cabal"];
        inherit (pkgs) cabal2nix;
     };

# run the cabal file
in
haskellPackages.callPackage cabal_nix_output {}
