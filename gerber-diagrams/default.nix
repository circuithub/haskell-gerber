{ mkDerivation, base, containers, diagrams-contrib, diagrams-lib
, gerber, lens, linear, mtl, pretty-show, recursion-schemes, stdenv
}:
mkDerivation {
  pname = "gerber-diagrams";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers diagrams-contrib diagrams-lib gerber lens linear
    mtl pretty-show recursion-schemes
  ];
  license = stdenv.lib.licenses.mit;
}
