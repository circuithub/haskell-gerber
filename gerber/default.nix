{ mkDerivation, base, containers, lens, linear, megaparsec, mtl
, pretty-show, recursion-schemes, stdenv, text, transformers
}:
mkDerivation {
  pname = "gerber";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens linear megaparsec mtl pretty-show
    recursion-schemes text transformers
  ];
  license = stdenv.lib.licenses.mit;
}
