{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    act.url = "github:ethereum/act/52e99daf3121a4e6a6cb28255e862cf8e83cf4cd";
    act.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc927;

          packages = {
            act.source = "${inputs.act}/src";
          };

          settings = {
            act.extraTestToolDepends = [pkgs.z3];
            poly = {
              # TODO: what's going on here? why are these tests failing? where is this dep even coming from?
              check = false;
              broken = false;
            };
          };

          # send everything but the devShell to the main flake outputs
          autoWire = [ "packages" "apps" "checks" ];
        };

        devShells.default = pkgs.mkShell {
          name = "og devshell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
