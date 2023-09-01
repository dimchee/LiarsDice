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
      overlay = self: super: {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ${compiler} = super.haskell.packages.${compiler}.override {
              overrides = final: prev: {${name} = prev.callCabal2nix name ./. { };};
            };
          };
        };
      };
      overlays = [overlay];
      normalPkgs = import nixpkgs {inherit overlays system; };
      survey = import "${static-haskell-nix}/survey" { inherit compiler normalPkgs; };
    in
      {
        packages.${system}.default = survey.haskellPackages.${name};
        # devShell.${system} = normalPkgs.mkShell {
        #   nativeBuildInputs = (survey.haskellPackages.callCabal2nix name ./. {}).nativeBuildInputs;
        #   buildInputs = 
        #   (survey.haskellPackages.callCabal2nix name ./. {}).buildInputs ++
        #   [
        #     normalPkgs.haskellPackages.cabal-fmt
        #     normalPkgs.haskellPackages.haskell-language-server
        #     normalPkgs.haskellPackages.fast-tags
        #   ];
        # };
      };
}
