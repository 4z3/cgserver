# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, attoparsec, filepath, httpTypes, ioStreams, safe
, text, wai, warp
}:

cabal.mkDerivation (self: {
  pname = "cgserver";
  version = "0.1.0.0";
  src = ./.;
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