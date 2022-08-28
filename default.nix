{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.haskellPackages.callCabal2nix "ljxml2md" ./. { }
