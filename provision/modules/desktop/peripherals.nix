{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # fuse filesystem implementation to access the contents of iOS devices
      pySVS # control SVS subwoofers from the command-line
      opensc # Open source smart card tools and middleware
      pcsc-tools # Tools are used to test a PC/SC drivers
      pkcs11helper # Library that simplifies the interaction with PKCS#11
    ];
    services.usbmuxd.enable = true; # for iOS mounting as storage
  };
}
