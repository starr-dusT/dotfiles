{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.desktop;
in
{
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # Fuse filesystem implementation to access the contents of iOS devices
      pySVS # Control SVS subwoofers from the command-line
    ];
    services.usbmuxd.enable = true; # for iOS mounting as storage
  };
}
