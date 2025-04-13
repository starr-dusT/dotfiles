{ lib, specialArgs, system, inputs, agenix, home-manager, ... }:
let
    user = specialArgs.user; 
    hostname = specialArgs.hostname;
in { 
  inherit system;
  specialArgs = { inherit user inputs home-manager; };
  modules = [
    ./configuration.nix
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
