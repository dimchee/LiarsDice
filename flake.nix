{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
	outputs = { self, nixpkgs }:
	let
		pkgs = import nixpkgs { system = "x86_64-linux"; };
		app = pkgs: pkgs.haskellPackages.developPackage {
			modifier = drv: pkgs.haskell.lib.overrideCabal drv (oa: {
					buildTools = (oa.buildTools or []) ++ [
						pkgs.haskellPackages.cabal-install
						pkgs.haskellPackages.cabal-fmt
						pkgs.haskellPackages.haskell-language-server
						pkgs.haskellPackages.fast-tags
					];
          enableSharedExecutables = false;
          enableSharedLibraries = false;
          configureFlags = [
            "--ghc-option=-optl=-static"
            "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${pkgs.zlib.static}/lib"
            "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
          ];
				});
			root = ./.;
		};
	in {
		defaultPackage.x86_64-linux = app pkgs.pkgsMusl;
		devShell.x86_64-linux = (app pkgs).env;
	};
}
