{
  description = "Flake for nixos configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = github:nix-community/home-manager/release-23.05;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jovian-nixos = {
      url = "git+https://github.com/Jovian-Experiments/Jovian-NixOS?ref=development";
      flake = false;
    };
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, jovian-nixos, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;
      user = "tstarr";
    in {
      nixosConfigurations = {
        kestrel = lib.nixosSystem {
          inherit system;
          specialArgs = { inherit user; inherit pkgs-unstable; };
          modules = [
            ./modules
            ./hosts/kestrel/configuration.nix
            ./hosts/kestrel/hardware.nix
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

        torus = lib.nixosSystem {
          inherit system;
          specialArgs = { inherit user; inherit pkgs-unstable; };
          modules = [
            ./modules
            ./hosts/torus/configuration.nix
            ./hosts/torus/hardware.nix
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
          specialArgs = { inherit user; inherit pkgs-unstable; inherit jovian-nixos; inherit home-manager; };
          modules = [
            ./modules
            ./hosts/bulwark/configuration.nix
            ./hosts/bulwark/hardware.nix
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
