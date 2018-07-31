{ mkDerivation, base, containers, foldl, generic-deriving
, megaparsec, monoid-extras, stdenv, text
}:
mkDerivation {
  pname = "gerber";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers foldl generic-deriving megaparsec monoid-extras
    text
  ];
  license = stdenv.lib.licenses.mit;
}
