{ pkgs ? import <nixpkgs> {}, devel ? false }:

with pkgs.haskellPackages;

cabal.mkDerivation (self: {
  pname = "cgserver";
  version = "0.1.0.0";
  src = if devel then ./. else pkgs.fetchgit {
    url = http://viljetic.de/~tv/git/cgserver;
    sha256 = "e48d52730d5c6eb190c9dd38b853c81441e3aae87b3ffbfeaa97cca28b94d8ce";
    rev = "d0129ff4d13cedbc6d1b85cadddc724783c8002a";
  };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec filepath httpTypes ioStreams safe text wai warp
  ];
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
