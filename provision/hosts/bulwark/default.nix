{ lib, system, user, inputs, agenix, home-manager, jovian-nixos, ... }:
{
  inherit system;
  specialArgs = { inherit user inputs; };
  modules = [
    ../default/physical/configuration.nix
    ./configuration.nix
    ./hardware.nix
    ../../modules
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit user; };
      home-manager.users.${user} = {
        imports = [ 
          ../../home-modules
        ];
      };
    }
  ];
}
