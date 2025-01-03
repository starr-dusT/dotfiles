{ lib, specialArgs, system, inputs, agenix, home-manager, ... }:
let
    user = specialArgs.user; 
    hostname = specialArgs.hostname;
in { 
  inherit system;
  specialArgs = { inherit user hostname inputs home-manager; };
  modules = [
    ../default                            # shared by all configs
    ../default/physical/configuration.nix # shared by physical machines
    ./configuration.nix                   # htpc specific
    ../../modules
    /etc/nixos/hardware-configuration.nix
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
    }
  ];
}
