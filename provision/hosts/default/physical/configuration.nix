{ config, pkgs, user, lib, inputs, ... }:
{
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" "openssl-1.1.1w" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  home-manager.users.${user} = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscode.fhs;
    };
  };
}
