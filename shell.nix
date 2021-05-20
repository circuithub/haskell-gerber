{ pkgs ? import <nixpkgs> { }, compiler ? "default" }:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  inherit (haskellPackages) callCabal2nix;

  gerber = callCabal2nix "gerber" ./gerber { };
  gerber-diagrams = callCabal2nix "gerber-diagrams" ./gerber-diagrams {
    inherit gerber;
  };

in
haskellPackages.shellFor {
  buildInputs = [ haskellPackages.cabal-install haskellPackages.haskell-language-server ];
  packages = p: [ gerber gerber-diagrams ];
}
