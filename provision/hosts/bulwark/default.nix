{ lib, system, user, inputs, agenix, home-manager, jovian-nixos, ... }:
{
  inherit system;
  specialArgs = { inherit user inputs home-manger; };
  modules = [
    ../default                            # shared by all configs
    ../default/physical/configuration.nix # shared by physical machines
    ./configuration.nix                   # bulwark specific
    ./hardware.nix
    ../../modules
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
    }
  ];
}
