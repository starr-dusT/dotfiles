{ config, lib, pkgs, user, jovian-nixos, home-manager, ... }:
{
  imports = [
    (jovian-nixos + "/modules")
    home-manager.nixosModule
  ];

  jovian = {
    steam.desktopSession = "gnome";
    steam.enable = true;
    steam.autoStart = true; 
    steam.user = "${user}";
    devices.steamdeck = {
      enable = true;
    };
  };

  # Disable gdm (this is required for Jovian-NixOS)
  services.xserver.displayManager.gdm.enable = lib.mkForce false;

  environment.systemPackages = with pkgs; [
    jupiter-dock-updater-bin # Binary package for updating firmware on Jupiter Dock, a hardware accessory for certain laptops.
    steamdeck-firmware # Firmware package for Valve's Steam Deck, a handheld gaming device.
  ];
}
