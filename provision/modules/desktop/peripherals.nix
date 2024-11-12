{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # fuse filesystem implementation to access the contents of iOS devices
      pySVS # control SVS subwoofers from the command-line
      (pkgs.writeScriptBin "sv" ''
        #!/bin/sh
        pySVS 54:B7:E5:57:1A:7B --volume="$1" && echo "$1" > /tmp/svs
      '')
      (pkgs.writeScriptBin "svv" ''
        #!/bin/sh
        pySVS 54:B7:E5:57:1A:7B --volume=A | grep -oP "(?<=VOLUME': )-?\\d+" > /tmp/svs
      '')
      opensc # Open source smart card tools and middleware
      pcsc-tools # Tools are used to test a PC/SC drivers
      pkcs11helper # Library that simplifies the interaction with PKCS#11
    ];
    services.usbmuxd.enable = true; # for iOS mounting as storage
  };
}
