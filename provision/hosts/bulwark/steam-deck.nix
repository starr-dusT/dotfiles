{ config, lib, pkgs, user, jovian-nixos, home-manager, ... }:
{
  imports = [
    (jovian-nixos + "/modules")
  ];

  jovian = {
    steam.desktopSession = "gnome";
    steam.enable = true;
    steam.user = "${user}";
    devices.steamdeck.enable = true;
  };

  environment.systemPackages = with pkgs; [
    jupiter-dock-updater-bin # Binary package for updating firmware on Jupiter Dock, a hardware accessory for certain laptops.
    steamdeck-firmware # Firmware package for Valve's Steam Deck, a handheld gaming device.
  ];
}
