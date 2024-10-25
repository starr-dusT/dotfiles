{ config, pkgs, user, lib, inputs, nixos-wsl, ... }:
let
  defaultUser = user;
in 
{
  imports = [ 
    nixos-wsl.nixosModules.wsl
  ];

  wsl = {
    inherit defaultUser;

    enable = true;
    wslConf.automount.root = "/mnt";
  };

  # Set networking options
  networking.hostName = "wsl"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  boot.isContainer = true;

  # configure root
  users.users.root = {
    extraGroups = [ "root" ];
  };
  security.sudo.wheelNeedsPassword = false;

  # Disable systemd units that don't make sense on WSL
  systemd.services."serial-getty@ttyS0".enable = false;
  systemd.services."serial-getty@hvc0".enable = false;
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@".enable = false;

  systemd.services.firewall.enable = false;
  systemd.services.systemd-resolved.enable = false;
  systemd.services.systemd-udevd.enable = false;

  # Don't allow emergency mode, because we don't have a console.
  systemd.enableEmergencyMode = false;

  environment.systemPackages = with pkgs; [
    firefox
    wget
  ];

  # Add DoD CA certs to trusted source
  security.pki.certificateFiles = [
    ./nipr.crt
  ];

  # Modules 
  modules = {
    programs = {
      chezmoi.apply = true;
      git.keys = false;
    };
    system = {
      terminal.enable = true;
    };
  };
}
