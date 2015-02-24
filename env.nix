{ pkgs ? import <nixpkgs> {} }:

let
  pname = "cgserver";
  version = "2";

  buildInputs = with pkgs; [
    hsEnv
  ];

  extraCmds = ''
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';

  hsEnv = pkgs.haskellngPackages.ghcWithPackages
    (self: with self;
      (callPackage ./cgserver.nix {}).nativeBuildInputs ++
      [
        cabal-install
      ]
    );

in pkgs.myEnvFun {
  name = "${pname}-${version}";
  inherit buildInputs extraCmds;
}
