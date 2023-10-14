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
    };
  };
}
