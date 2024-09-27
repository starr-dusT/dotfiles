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
  in {
    nixosConfigurations = ( 
      import ./hosts/kestrel {
        inherit (nixpkgs) lib;
        inherit system user inputs agenix home-manager;
      }
      import ./hosts/shivan {
        inherit (nixpkgs) lib;
        inherit system user inputs agenix home-manager;
      }
      import ./hosts/torus {
        inherit (nixpkgs) lib;
        inherit system user inputs agenix home-manager;
      }
      import ./hosts/bulwark {
        inherit (nixpkgs) lib;
        inherit system user inputs agenix home-manager jovian-nixos;
      }
    ); 
  };
}
