{ mkDerivation, base, containers, foldl, generic-deriving, lens
, linear, megaparsec, monoid-extras, mtl, pretty-show
, recursion-schemes, stdenv, text, transformers
}:
mkDerivation {
  pname = "gerber";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers foldl generic-deriving lens linear megaparsec
    monoid-extras mtl pretty-show recursion-schemes text transformers
  ];
  license = stdenv.lib.licenses.mit;
}
