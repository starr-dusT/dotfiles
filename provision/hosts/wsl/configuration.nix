{ config, pkgs, user, lib, inputs, nixos-wsl, hostname, ... }:
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

  # Enable docker 
  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [
    firefox
    wget
    openssl
  ];

  # Add DoD CA certs to trusted source
  security.pki.certificateFiles = [
    ../../../resources/dod_certs/DoDWCFInterCA1.crt
    ../../../resources/dod_certs/DoDRootCA3.crt
    ../../../resources/dod_certs/DoDRootCA4.crt
    ../../../resources/dod_certs/DoDRootCA5.crt
    ../../../resources/dod_certs/DoDRootCA6.crt
    ../../../resources/dod_certs/DoDInteroperabilityRootCA2.crt
    ../../../resources/dod_certs/USDoDCCEBInteroperabilityRootCA2.crt
  ];

  # Modules 
  modules = {
    devel.programming.enable = true;
    programs = {
      git.keys = false;
    };
    services.ssh.enable = true;
  };
}
