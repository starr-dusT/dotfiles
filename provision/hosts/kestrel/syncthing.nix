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
    configDir = "/home/${user}/.config/syncthing";
    guiAddress = "0.0.0.0:8384";
    overrideDevices = true;
    overrideFolders = true; 
    settings.devices = {
      "bulwark" = { id = "ZGLQ725-OJSDNTE-MXYLIUD-XDB7REJ-2B2DVNU-PAFF6VC-MUUWRI6-4SNPWAK"; };
      "torus" = { id = "WCZYHD7-5Y33SSU-74JHAQR-V7LYMDM-SDG2NTN-DJ2VKF2-DUBBUE5-PU5CGQN"; };
      "shivan" = { id = "KUSOQSH-RKLLA32-T3KAPQP-VNJISLL-QSQCGFZ-ZL7ZULE-MJC67DK-2U6G4Q7"; };
    };
    settings.folders = {
      "Gamecube Saves" = {
        path = "/home/${user}/.local/share/dolphin-emu/GC";
        devices = [ "bulwark" ];
      };
      "Ryujinx Saves" = {
        path = "/home/${user}/.config/Ryujinx/bis";
        devices = [ "bulwark" ];
      };
      "Yuzu Saves" = {
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
      "General Sync" = {
        path = "/home/${user}/sync";
        devices = [ "shivan" ];
      };
    };
  };
}
