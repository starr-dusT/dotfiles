{ lib, specialArgs, system, user, inputs, agenix, home-manager, ... }:
let
    user = specialArgs.user; 
in { 
  inherit system;
  specialArgs = { inherit user inputs home-manager; };
  modules = [
    ../default                            # shared by all configs
    ../default/physical/configuration.nix # shared by physical machines
    ./configuration.nix                   # torus specific
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
