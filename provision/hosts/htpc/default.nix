{ lib, specialArgs, system, inputs, agenix, home-manager, nix-flatpak, ... }:
let
    user = specialArgs.user; 
    hostname = specialArgs.hostname;
in { 
  inherit system;
  specialArgs = { inherit user hostname inputs home-manager nix-flatpak; };
  modules = [
    ./configuration.nix
    ../../modules
    /etc/nixos/hardware-configuration.nix
    agenix.nixosModules.default
    nix-flatpak.nixosModules.nix-flatpak
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
    }
  ];
}
