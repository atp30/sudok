{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:
with pkgs;
stdenv.mkDerivation {
  name = "sudoku-build-environment";
  buildInputs = [
   cabal2nix
   nix
   (haskellPackages.ghcWithHoogle
        (haskellPackages:
            # with haskellPackages;
            # [cabal-install classy-prelude]))
         builtins.concatLists
         [
         (callPackage ./default.nix {}).buildInputs
         [cabal-install]
         ]))
    ];

  shellHook = ''
    PATH=`cat *.cabal\
        | grep executable\
        | sed -E 's/executable\s*(\w*)\s*\n?/.\/dist\/build\/\1:/'\
        | tr -d '\n'`:$PATH
   '';
}
