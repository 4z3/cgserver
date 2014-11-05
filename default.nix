# This file is just a convenient wrapper around cgserver.nix,
# so the package can be installed by:
#
#     nix-env -f . -i
#

{ pkgs ? import <nixpkgs> {}
}:

pkgs.haskellPackages.callPackage ./cgserver.nix {}
