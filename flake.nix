# file: flake.nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc927;

          settings = {
            # TODO: whats going on here?
            poly.check = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        devShells.default = pkgs.mkShell {
          name = "open games devshell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            # other development tools.
          ];
        };
      };
    };
}
