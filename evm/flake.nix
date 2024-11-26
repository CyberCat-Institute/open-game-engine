{
  inputs = {
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, solc }: let
    pkgs = import nixpkgs {
      system = "aarch64-darwin"; # Apple Silicon architecture
      overlays = [
        solc.overlay
      ];
    };
  in {
    devShell.aarch64-darwin = with pkgs; mkShell {
      buildInputs = [
        solc_0_4_26
        solc_0_7_6
        solc_0_8_19
        (solc.mkDefault pkgs solc_0_8_26)
      ];
    };
  };
}

