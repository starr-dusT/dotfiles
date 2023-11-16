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
      "Gamecube Saves" = {
        path = "/home/${user}/.local/share/dolphin-emu/GC";
        devices = [ "kestrel" ];
      };
      "Switch Saves" = {
        path = "/home/${user}/.local/share/yuzu/nand/user/save/0000000000000000/705C6CE0127692D598F92E68B640D644";
        devices = [ "kestrel" ];
      };
      "PSP Saves" = {
        path = "/home/${user}/.config/ppsspp/PSP/SAVEDATA";
        devices = [ "kestrel" ];
      };
      "PSP Save State" = {
        path = "/home/${user}/.config/ppsspp/PSP/PPSSPP_STATE";
        devices = [ "kestrel" ];
      };
    };
  };
}
