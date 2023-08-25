{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
	outputs = { self, nixpkgs }:
	let
		pkgs = import nixpkgs { system = "x86_64-linux"; };
		app = pkgs.haskellPackages.developPackage {
			modifier = drv: pkgs.haskell.lib.overrideCabal drv (oa: {
					buildTools = (oa.buildTools or []) ++ [
						pkgs.haskellPackages.cabal-install
						pkgs.haskellPackages.cabal-fmt
						pkgs.haskellPackages.haskell-language-server
						pkgs.haskellPackages.fast-tags
					];
				});
			root = ./.;
		};
	in {
		defaultPackage.x86_64-linux = app;
		devShell.x86_64-linux = app.env;
	};
}
