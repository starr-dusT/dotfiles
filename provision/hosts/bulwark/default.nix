{ lib, system, user, inputs, agenix, home-manager, jovian-nixos, ... }:
{
  kestrel = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit user inputs; };
    modules = [
      ../default
      ../../modules
      ./configuration.nix
      ./hardware.nix
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
  };
}
