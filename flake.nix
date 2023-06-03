{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {self, ...}@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = inputs.nixpkgs.legacyPackages."${system}";
      in {
      packages.svg-filter =
        pkgs.haskellPackages.callCabal2nix "pandoc-step-filter" ./. {};
      packages.default = self.packages."${system}".svg-filter;
      apps.default = self.apps.${system}.pandoc-step-filter;
      apps.pandoc-step-filter = "${self.packages."${system}".pandoc-step-filter}/bin/pandoc-step-filter";

      devShells.default =
        pkgs.mkShell {
          packages = [ pkgs.haskellPackages.pandoc ];
          inputsFrom = [ self.packages.${system}.svg-filter ];
        };
    });
}
