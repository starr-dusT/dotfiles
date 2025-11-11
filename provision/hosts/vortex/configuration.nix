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
      development = {
        k3s.enable = true;
      };
      programs = {
        programming.enable = true;
      };
    };
  };
}
