{
  description = "A Simple Liars Dice Simulator";

  inputs = {
    nixpkgs = {
      url = "https://github.com/nh2/nixpkgs/archive/9e49f8f1f37bc906cda1adb33064c325d760819a.tar.gz";
      type = "tarball";
      flake = false;
    };
    static-haskell-nix = {
      url = "github:nh2/static-haskell-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, static-haskell-nix} :
    let
      compiler = "ghc927";
      name = "LiarsDice";
      system = "x86_64-linux";
      overlays = [ 
        (self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${compiler} = super.haskell.packages.${compiler}.override {
                overrides = final: prev: {${name} = prev.callCabal2nix name ./. { };};
              };
            };
          };
        })
      ];
      normalPkgs = import nixpkgs {inherit overlays system; };
      haskellPackages = normalPkgs.haskell.packages.${compiler};
      survey = import "${static-haskell-nix}/survey" { inherit compiler normalPkgs; };
    in
      {
        packages.${system}.default = survey.haskellPackages.${name};
        devShell.${system} = normalPkgs.mkShell {
          inputsFrom = [
            (haskellPackages.callCabal2nix name ./. {}).env
          ];
          buildInputs = [
            haskellPackages.cabal-fmt
            haskellPackages.fast-tags
            haskellPackages.haskell-language-server
          ];
        };
      };
}
