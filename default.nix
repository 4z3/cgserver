{ pkgs ? import <nixpkgs> {}
}:

pkgs.haskellPackages.callPackage ./cgserver.nix {}
