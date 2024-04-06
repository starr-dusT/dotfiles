{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.gnome;
in {
  #imports = [
  #  home-manager.nixosModule
  #];

  options.modules.desktop.gnome = with lib; {
    enable = lib.mkEnableOption "gnome";
    #privateKeyFile = lib.mkOption { type = with types; str; };
    #address = lib.mkOption { type = with types; listOf str; };
    #publicKey = lib.mkOption { type = with types; str; };
    #endpoint = lib.mkOption { type = with types; str; };
    #autostart = lib.mkOption {
    #  type = with types; bool;
    #  default = false;
    #};
  };

  config = lib.mkIf cfg.enable {
    # Enable wayland gnome
    services.xserver = {
      enable = true;  
      displayManager.gdm = {
        enable = true;  
        wayland = true; 
      };
      desktopManager.gnome.enable = true; 
    };

    # Enable sound and handle conflict (https://github.com/Jovian-Experiments/Jovian-NixOS/issues/99)
    sound.enable = true;
    hardware.pulseaudio.enable = lib.mkForce false;

    ## GNOME settings through home
    #home-manager.users.${user} = {
    #  dconf.settings = {
    #    "org/gnome/desktop/background" = {
    #      picture-options = "centered";
    #      picture-uri = "file://${../../../resources/img/bulwark.png}";
    #      picture-uri-dark = "file://${../../../resources/img/bulwark.png}";
    #    };
    #    # Enable on-screen keyboard
    #    "org/gnome/desktop/a11y/applications" = {
    #      screen-keyboard-enabled = true;
    #    };
    #    "org/gnome/shell" = {
    #      favorite-apps = [
    #        "steam.desktop"
    #        "org.gnome.Console.desktop"
    #        "chromium-browser.desktop"
    #      ];
    #    };
    #  };
    #};
  };
}
