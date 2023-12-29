{
  description = "Opt-in Stack Flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.solc.url = "github:hellwolf/solc.nix";

  outputs = { self, nixpkgs, flake-utils, solc }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc.overlay
          ];
        };

        hPkgs = pkgs.haskell.packages."ghc945"; # need to match Stackage LTS version from stack.yaml resolver

        minimalDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          pkgs.zlib # External C library needed by some Haskell packages
          stack-wrapped
          pkgs.solc_0_8_2
        ];

        extraDevTools = with hPkgs; [
          haskell-language-server # LSP server for editor
          implicit-hie # auto generate LSP hie.yaml file from cabal
        ];

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = minimalDevTools ++ extraDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath minimalDevTools;
        };
        devShells.minimal = pkgs.mkShell {
          buildInputs = minimalDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath minimalDevTools;
        };
      });
}

