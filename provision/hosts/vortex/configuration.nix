{ pkgs, ... }:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set networking options
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };

    optional = {
      programs = {
        docker = {
          enable = true;
          storageDriver = "btrfs";
        };
      };
      scripts.enable = true;
    };
  };
}
