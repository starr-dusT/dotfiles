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
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-contrib = {
        url = "github:hyprwm/contrib";
        inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, jovian-nixos, agenix, hyprland, ... }:
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
          ./modules
          ./hosts/kestrel/configuration.nix
          ./hosts/kestrel/hardware.nix
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
                ./hosts/kestrel/home-configuration.nix 
              ];
            };
          }
        ];
      };

      shivan = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; };
        modules = [
          ./modules
          ./hosts/shivan/configuration.nix
          ./hosts/shivan/hardware.nix
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
                ./hosts/shivan/home-configuration.nix 
              ];
            };
          }
        ];
      };

      torus = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; };
        modules = [
          ./modules
          ./hosts/torus/configuration.nix
          ./hosts/torus/hardware.nix
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
                ./hosts/torus/home-configuration.nix 
              ];
            };
          }
        ];
      };

      bulwark = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit user; inherit inputs; inherit jovian-nixos; inherit home-manager; };
        modules = [
          ./modules
          ./hosts/bulwark/configuration.nix
          ./hosts/bulwark/hardware.nix
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit user; };
            home-manager.users.${user} = {
              imports = [ 
                ./home-modules
                ./hosts/bulwark/home-configuration.nix 
              ];
            };
          }
        ];
      };
    };
  };
}
