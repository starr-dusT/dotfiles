{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  environment.systemPackages = with pkgs; [ 
    syncthing # File sync program for multiple devices in real-time.
  ];

  services.syncthing = {
    enable = true;
    user = "${user}";
    dataDir = "/home/${user}/.local/share/syncthing";
    guiAddress = "0.0.0.0:8384";
    overrideDevices = true;
    overrideFolders = true; 
    settings.devices = {
      "kestrel" = { id = "KYEWTBL-GL343U7-OIM63LT-2IYGJAP-RCL545L-2KJOIY4-6352W6Y-DZRVGAL"; };
    };
    settings.folders = {
      "Gamecube Saves" = {
        path = "/home/${user}/.local/share/dolphin-emu/GC";
        devices = [ "kestrel" ];
      };
      "Ryujinx Saves" = {
        path = "/home/${user}/.config/Ryujinx/bis";
        devices = [ "kestrel" ];
      };
      "Yuzu Saves" = {
        path = "/home/${user}/.local/share/yuzu/nand/user/save/0000000000000000/705C6CE0127692D598F92E68B640D644";
        devices = [ "kestrel" ];
      };
      "PSP Saves" = {
        path = "/home/${user}/.config/ppsspp/PSP/SAVEDATA";
        devices = [ "kestrel" ];
      };
      "PSP Save States" = {
        path = "/home/${user}/.config/ppsspp/PSP/PPSSPP_STATE";
        devices = [ "kestrel" ];
      };
    };
  };
}
