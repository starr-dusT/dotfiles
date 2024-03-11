{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  environment.systemPackages = with pkgs; [ syncthing ];

  services.syncthing = {
    enable = true;
    user = "${user}";
    dataDir = "/home/${user}/.local/share/syncthing";
    configDir = "/home/${user}/.config/syncthing";
    guiAddress = "0.0.0.0:8384";
    overrideDevices = true;
    overrideFolders = true; 
    settings.devices = {
      "kestrel" = { id = "KYEWTBL-GL343U7-OIM63LT-2IYGJAP-RCL545L-2KJOIY4-6352W6Y-DZRVGAL"; };
    };
    settings.folders = {
      "General Sync" = {
        path = "/home/${user}/sync";
        devices = [ "kestrel" ];
      };
    };
  };
}
