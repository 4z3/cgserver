{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, filepath, http-types, io-streams, safe, stdenv, text
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "cgserver";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bytestring containers directory filepath
    http-types io-streams safe text wai wai-extra warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
