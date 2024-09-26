{
  description = "Flake to manage my nixos machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jovian-nixos = {
      url = "git+https://github.com/Jovian-Experiments/Jovian-NixOS?ref=development";
      flake = false;
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, jovian-nixos, agenix, ... }:
  let
    system = "x86_64-linux";
    user = "tstarr";
    lib = nixpkgs.lib;
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    nixosConfigurations = {
      kestrel = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; };
        modules = [
          ./hosts/default.nix
          ./hosts/kestrel/configuration.nix
          ./hosts/kestrel/hardware.nix
          ./modules
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
              ];
            };
          }
        ];
      };

      shivan = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; };
        modules = [
          ./hosts/default.nix
          ./hosts/shivan/configuration.nix
          ./hosts/shivan/hardware.nix
          ./modules
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
              ];
            };
          }
        ];
      };

      torus = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; };
        modules = [
          ./hosts/default.nix
          ./hosts/torus/configuration.nix
          ./hosts/torus/hardware.nix
          ./modules
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
              ];
            };
          }
        ];
      };

      bulwark = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; inherit jovian-nixos; inherit home-manager; };
        modules = [
          ./hosts/default.nix
          ./hosts/bulwark/configuration.nix
          ./hosts/bulwark/hardware.nix
          ./modules
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
              ];
            };
          }
        ];
      };
    };
  };
}
