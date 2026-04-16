{ mkDerivation
, base
, diagrams-cairo
, diagrams-lib
, foldl
, gerber
, linear
, optparse-applicative
, rio
, stdenv
, text
}:
mkDerivation {
  pname = "gerber-diagrams";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base diagrams-lib gerber linear ];
  executableHaskellDepends = [
    base
    diagrams-cairo
    diagrams-lib
    foldl
    gerber
    optparse-applicative
    rio
    text
  ];
  license = stdenv.lib.licenses.mit;
}
