{ config, lib, pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [ syncthing ];
  services.syncthing = {
    enable = true;
    user = "${user}";
    configDir = "/home/${user}/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true; 
    devices = {
    };
    folders = {
    };
  };
}
