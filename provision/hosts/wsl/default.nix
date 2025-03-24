{ lib, specialArgs, system, inputs, agenix, home-manager, nixos-wsl, ... }:
let
    user = specialArgs.user; 
    hostname = specialArgs.hostname;
in { 
  inherit system;
  specialArgs = { inherit user hostname inputs nixos-wsl home-manager; };
  modules = [
    ../default          # shared by all configs
    ./configuration.nix # wsl specific
    ../../modules
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
    }
  ];
}
