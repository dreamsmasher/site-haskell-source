{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Naomi's blog";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
      ghcVer = "ghc902";
    in
    {
      overlay = (final: prev: {
        nliu-net = final.haskell.packages.${ghcVer}.callCabal2nix "nliu-net" ./. {};
      });
      packages = forAllSystems (system: {
         nliu-net = nixpkgsFor.${system}.nliu-net;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.nliu-net);
      checks = self.packages;

      # for debugging
      inherit nixpkgsFor;

      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskell.packages.${ghcVer};
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.nliu-net];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            stack
            stylish-haskell
            hasktags
          ];
        # Change the prompt to show that you are in a devShell
        # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
