let
  pkgs = import
    (fetchTarball {
      name = "release-23.11";
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";
      sha256 = sha256:1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k;
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
