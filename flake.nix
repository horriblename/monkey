{
  description = "A very basic flake";

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    packages.${system}.default = pkgs.callPackage ./default.nix;

    devShell.${system} = pkgs.mkShell {
      name = "rust shell";
      packages = with pkgs; [cargo];
    };
  };
}
