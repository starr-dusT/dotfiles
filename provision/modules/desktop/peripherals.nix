{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.peripherals;
in {
  options.modules.desktop.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {
    services.usbmuxd.enable = true; # for iOS mounting as storage
    environment.systemPackages = with pkgs; [
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # fuse filesystem implementation to access the contents of iOS devices
      pySVS # control SVS subwoofers from the command-line
    ];
  };
}
