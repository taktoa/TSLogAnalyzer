{ mkDerivation, attoparsec, base, classy-prelude, configurator
, containers, extra, fingertree, hashable, ListLike, mtl, stdenv
, text, text-icu, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "TSLogAnalyzer";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base classy-prelude configurator containers extra
    fingertree hashable ListLike mtl text text-icu time transformers
    unordered-containers
  ];
  executableHaskellDepends = [ base ];
  description = "Analyzes TeamSpeak 3 server log files";
  license = stdenv.lib.licenses.mit;
}
