{ pkgs ? import <nixpkgs> {}
}:

let
  name = "cgserver";
  version = "1";

  buildInputs = with pkgs; [
    hsEnv
  ];

  extraCmds = ''
    if test -d "\$HOME/.history"; then
      export HISTFILE="\$HOME/.history/env-${name}"
    fi
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';

  hsEnv = pkgs.haskellPackages_ghc783_profiling.ghcWithPackages
    (self : with self;
      (callPackage ./cgserver.nix {}).nativeBuildInputs ++
      [
        cabalInstall
      ]
    );

in pkgs.myEnvFun {
  name = "${name}-${version}";
  inherit buildInputs extraCmds;
}
