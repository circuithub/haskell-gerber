let
  pkgs = import
    (fetchTarball {

      # Note we should make sure everything builds with lower bound and upper bound package
      # set which requires having some warnings in when compiling in both, e.g. in the lower
      # bound we need to import Data.List (foldl') and in the upper bound this is a warning
      # for an unused import. Technically I guess we could use CPP to clean this up but that
      # is more work.

      # the current upper bound
      name = "release-25.11";
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.11.tar.gz";
      sha256 = sha256:1zn1lsafn62sz6azx6j735fh4vwwghj8cc9x91g5sx2nrg23ap9k;

      # # the original lower bound for packages
      # name = "release-23.11";
      # url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";
      # sha256 = sha256:1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k;
    })
    { config = { allowBroken = true; }; };

  haskellPackages =
    pkgs.haskellPackages;

  inherit (haskellPackages) callCabal2nix;

  gerber = callCabal2nix "gerber" ./gerber { };
  gerber-diagrams = callCabal2nix "gerber-diagrams" ./gerber-diagrams {
    inherit gerber;
  };


  ch-hs-imports =
    pkgs.haskellPackages.callPackage
      ({ mkDerivation
       , abstract-par
       , base
       , containers
       , deepseq
       , directory
       , dlist
       , fetchgit
       , filepath
       , lib
       , megaparsec
       , monad-par
       , monad-par-extras
       , monoidal-containers
       , mtl
       , nonempty-containers
       , optparse-applicative
       , process
       , text
       , transformers
       }:
        mkDerivation {
          pname = "ch-hs-imports";
          version = "0.1.0.0";
          src = fetchgit {
            url = "https://github.com/circuithub/ch-hs-imports";
            sha256 = "1ayg2pbzgy3zwb14f084s4rbp0qaxn6fygg3l4avj7pc7kxb1z2j";
            rev = "7e5b0bc5bc9dde28015784e9a61270a031a126dc";
            fetchSubmodules = true;
          };
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            abstract-par
            base
            containers
            deepseq
            directory
            dlist
            filepath
            megaparsec
            monad-par
            monad-par-extras
            monoidal-containers
            mtl
            nonempty-containers
            optparse-applicative
            process
            text
            transformers
          ];
          jailbreak = true;
          license = lib.licenses.mit;
          mainProgram = "ch-hs-imports";
        })
      { };

in
haskellPackages.shellFor {
  buildInputs =
    [
      haskellPackages.cabal-install
      pkgs.treefmt
      pkgs.nixpkgs-fmt
      haskellPackages.haskell-language-server
      haskellPackages.fourmolu
      haskellPackages.cabal-fmt
      ch-hs-imports
    ];
  packages = p: [ gerber gerber-diagrams ];
}
