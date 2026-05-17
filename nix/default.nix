{ pkgs }:

let
  haskellPackages = pkgs.haskellPackages;
in
haskellPackages.callCabal2nix "tiramisu" ../. { }
