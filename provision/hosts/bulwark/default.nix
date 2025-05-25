{ lib, specialArgs, system, inputs, agenix, home-manager, jovian-nixos, nix-flatpak, ... }:
let
    user = specialArgs.user; 
    hostname = specialArgs.hostname;
in { 
  inherit system;
  specialArgs = { inherit user hostname inputs home-manager jovian-nixos nix-flatpak; };
  modules = [
    ./configuration.nix
    ./hardware.nix
    ../../modules
    agenix.nixosModules.default
    nix-flatpak.nixosModules.nix-flatpak
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
    }
  ];
}
