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
      "bulwark" = { id = "B5HZK2V-WA4WSQF-3JAIH4I-C6XQZ6J-EMMAIV5-CCYOA5G-N57GT6A-WH2GCQ2"; };
    };
    folders = {
      "Gamecube Saves" = {
        path = "/home/${user}/.local/share/dolphin-emu/GC";
        devices = [ "bulwark" ];
      };
      "Switch Saves" = {
        path = "/home/${user}/.local/share/yuzu/nand/user/save/0000000000000000/705C6CE0127692D598F92E68B640D644";
        devices = [ "bulwark" ];
      };
    };
  };
}
