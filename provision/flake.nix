{
  description = "Flake to manage my nixos machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=v0.6.0";
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      agenix,
      nix-flatpak,
      ...
    }:
    let
      system = "x86_64-linux";
      hosts = builtins.fromJSON (builtins.readFile ./hosts.json);
      lib = nixpkgs.lib;
    in
    {
      nixosConfigurations = lib.mapAttrs (
        hostname: hostConfig:
        lib.nixosSystem {
          specialArgs = {
            inherit
              lib
              system
              inputs
              agenix
              home-manager
              nix-flatpak
              ;
            user = hostConfig.user;
            hostname = "${hostname}";
          };
          modules = [
            ./hosts/${hostConfig.role}/configuration.nix
            ./modules
            agenix.nixosModules.default
            nix-flatpak.nixosModules.nix-flatpak
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                user = hostConfig.user;
              };
            }
          ]
          ++ [
            (
              if builtins.pathExists ./hosts/${hostConfig.role}/hardware.nix then
                ./hosts/${hostConfig.role}/hardware.nix
              else
                /etc/nixos/hardware-configuration.nix
            )
          ];
        }
      ) hosts;
    };
}
