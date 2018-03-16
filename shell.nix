{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, lens, linear, megaparsec
      , mtl, pretty-show, recursion-schemes, stdenv, text, transformers
      , diagrams
      }:
      mkDerivation {
        pname = "gerber";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers lens linear megaparsec mtl pretty-show
          recursion-schemes text transformers diagrams
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
