{ pkgs, ... }:
{
  imports = [
    ./adguard.nix
    ./cloudflared.nix
    ./home-assistant
    ./netboot.nix
    ./networking.nix
    ./wireguard-server.nix
  ];

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
        programming.enable = true;
      };
    };
  };
}
