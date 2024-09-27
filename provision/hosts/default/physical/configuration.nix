{ config, pkgs, user, lib, inputs, ... }:
{
  imports = [ 
    ./home-configuration.nix
    ./backup.nix
  ];

  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" "openssl-1.1.1w" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" "docker" "libvirtd" ];
    shell = pkgs.bash;
  };
}
