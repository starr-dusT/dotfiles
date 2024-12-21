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
    hosts = builtins.fromJSON (builtins.readFile ./hosts.json);
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = lib.mapAttrs (hostname: hostConfig:
      lib.nixosSystem (import ./hosts/${hostname} {
        inherit lib;
        inherit system inputs agenix home-manager jovian-nixos nixos-wsl;
        specialArgs = {
          user = hostConfig.user;
          hostname = "${hostname}";
        };
      })
    ) hosts;
  };
}
