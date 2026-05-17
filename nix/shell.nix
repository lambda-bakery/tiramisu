{
  pkgs ? import <nixpkgs> { },
}:

let
  haskellPackages = pkgs.haskellPackages;
in
pkgs.mkShell {
  packages = with haskellPackages; [
    ghc
    cabal-install
    cabal-gild
    cabal-fmt
    haskell-language-server
    ormolu
    pkgs.nixfmt
  ];
}
