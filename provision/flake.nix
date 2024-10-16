{
  description = "Flake to manage my nixos machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    jovian-nixos.url = "git+https://github.com/Jovian-Experiments/Jovian-NixOS?ref=development";
    jovian-nixos.flake = false;
    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, jovian-nixos, agenix, nixos-wsl, ... }:
  let
    system = "x86_64-linux";
    user = "tstarr";
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = {
      kestrel = lib.nixosSystem (import ./hosts/kestrel {
        inherit lib;
        inherit system user inputs agenix home-manager;
      });
      shivan = lib.nixosSystem (import ./hosts/shivan {
        inherit lib;
        inherit system user inputs agenix home-manager;
      });
      torus = lib.nixosSystem (import ./hosts/torus {
        inherit lib;
        inherit system user inputs agenix home-manager;
      });
      bulwark = lib.nixosSystem (import ./hosts/bulwark {
        inherit lib;
        inherit system user inputs agenix home-manager jovian-nixos;
      });
      wsl = lib.nixosSystem (import ./hosts/wsl {
        inherit lib;
        inherit system user inputs agenix home-manager nixos-wsl;
      });
      osprey = lib.nixosSystem (import ./hosts/osprey {
        inherit lib;
        inherit system user inputs agenix home-manager;
      });
    }; 
  };
}
