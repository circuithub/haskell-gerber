let
  pkgs = import
    (fetchTarball {
      name = "release-23.05";
      url = "https://github.com/nixos/nixpkgs/archive/3238fc2e4884c03975fd0c832adbd3cc3d7fe65c.tar.gz";
      sha256 = "1s84shi6jb7vhcvv9wvyskcwl4i46rnjwfpqdiwjy3hil0h9iim4";
    })
    { config = { allowBroken = true; }; };

  haskellPackages =
    pkgs.haskellPackages;

  inherit (haskellPackages) callCabal2nix;

  gerber = callCabal2nix "gerber" ./gerber { };
  gerber-diagrams = callCabal2nix "gerber-diagrams" ./gerber-diagrams {
    inherit gerber;
  };

in
haskellPackages.shellFor {
  buildInputs = [ haskellPackages.cabal-install ];
  packages = p: [ gerber gerber-diagrams ];
}
