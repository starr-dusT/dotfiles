{ config, lib, pkgs, user, jovian-nixos, home-manager, ... }:
{
  imports = [
    (jovian-nixos + "/modules")
    home-manager.nixosModule
  ];

  jovian = {
    steam.desktopSession = "gnome-xorg";
    steam.enable = true;
    steam.autoStart = true; 
    steam.user = "${user}";
    devices.steamdeck = {
      enable = true;
    };
  };

  # Enable GNOME
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = false;

  # Enable sound and handle conflict (https://github.com/Jovian-Experiments/Jovian-NixOS/issues/99)
  sound.enable = true;
  hardware.pulseaudio.enable = lib.mkForce false;

  environment.systemPackages = with pkgs; [
    jupiter-dock-updater-bin
    steamdeck-firmware
  ];

  # GNOME settings through home
  home-manager.users.${user} = {
    dconf.settings = {
      "org/gnome/desktop/background" = {
        picture-uri = "file://${../../../resources/img/bulwark.png}";
      };
      # Enable on-screen keyboard
      "org/gnome/desktop/a11y/applications" = {
        screen-keyboard-enabled = true;
      };
      "org/gnome/shell" = {
        favorite-apps = [
          "steam.desktop"
          "org.gnome.Console.desktop"
          "chromium-browser.desktop"
        ];
      };
    };
  };
}
