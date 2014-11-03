let
  name = "cgroup-server";
  version = "1";
  buildInputs = with pkgs; [
    hsEnv
  ];
  extraCmds = with pkgs; ''
    export HISTFILE="\$HOME/.history/env-${name}"
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';
  pkgs = import <nixpkgs> {} // (with pkgs; {
  });
  hsEnv = pkgs
      .haskellPackages_ghc783_profiling
      .ghcWithPackages (hsPkgs: with hsPkgs;
    let
    in
      (callPackage ./. { devel = true; }).nativeBuildInputs ++ [
      cabalInstall
    ]);
in pkgs.myEnvFun {
  name = "${name}-${version}";
  inherit buildInputs extraCmds;
}
# vim: set fdm=marker :
