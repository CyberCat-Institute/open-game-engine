{
  inputs = {
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, solc }: let
    pkgs = import nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        solc.overlay
      ];
    };
  in {
    devShell.aarch64-darwin = with pkgs; mkShell {
      buildInputs = [
        (solc.mkDefault pkgs solc_0_8_26)
      ];
    };
  };
}

