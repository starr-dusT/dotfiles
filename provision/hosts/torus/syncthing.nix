{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  environment.systemPackages = with pkgs; [ syncthing ];

  services.syncthing = {
    enable = true;
    user = "${user}";
    configDir = "/home/${user}/.config/syncthing";
    guiAddress = "0.0.0.0:8384";
    overrideDevices = true;
    overrideFolders = true; 
    devices = {
      "kestrel" = { id = "TY6I6UK-YWXZYB4-7DKSB5Y-6ZBGE6U-T5WNJK4-KPLTXP7-ZTZQPXU-LX4HPQZ"; };
    };
    folders = {
      "Vault" = {
        path = "/engi/apps/dufs/vault";
        devices = [ "kestrel" ];
      };
    };
  };
}
