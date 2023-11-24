{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  environment.systemPackages = with pkgs; [ syncthing ];

  services.syncthing = {
    enable = true;
    user = "${user}";
    dataDir = "/home/${user}";
    configDir = "/home/${user}/.config/syncthing";
    guiAddress = "0.0.0.0:8384";
    overrideDevices = true;
    overrideFolders = true; 
    settings.devices = {
      "bulwark" = { id = "B5HZK2V-WA4WSQF-3JAIH4I-C6XQZ6J-EMMAIV5-CCYOA5G-N57GT6A-WH2GCQ2"; };
      "torus" = { id = "WCZYHD7-5Y33SSU-74JHAQR-V7LYMDM-SDG2NTN-DJ2VKF2-DUBBUE5-PU5CGQN"; };
    };
    settings.folders = {
      "Gamecube Saves" = {
        path = "/home/${user}/.local/share/dolphin-emu/GC";
        devices = [ "bulwark" ];
      };
      "Switch Saves" = {
        path = "/home/${user}/.local/share/yuzu/nand/user/save/0000000000000000/705C6CE0127692D598F92E68B640D644";
        devices = [ "bulwark" ];
      };
      "PSP Saves" = {
        path = "/home/${user}/.config/ppsspp/PSP/SAVEDATA";
        devices = [ "bulwark" ];
      };
      "PSP Save States" = {
        path = "/home/${user}/.config/ppsspp/PSP/PPSSPP_STATE";
        devices = [ "bulwark" ];
      };
    };
  };
}
