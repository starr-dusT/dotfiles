{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # Fuse filesystem implementation to access the contents of iOS devices
      pySVS # Control SVS subwoofers from the command-line
      (pkgs.writeScriptBin "sv" ''
        #!/bin/sh
        pySVS 54:B7:E5:57:1A:7B --volume="$1" && echo "$1" > /tmp/svs
      '')
      (pkgs.writeScriptBin "svv" ''
        #!/bin/sh
        pySVS 54:B7:E5:57:1A:7B --volume=A | grep -oP "(?<=VOLUME': )-?\\d+" > /tmp/svs
      '')
      (pkgs.writeScriptBin "ss" ''
        #!/bin/sh
        sink=$(wpctl status | awk '/Audio/{flag=1} /Video/{flag=0} flag' | awk '/Sinks:/{flag=1; next} /Sources:/{flag=0} flag' | grep -E "$1" | awk '{for(i=1;i<=NF;i++) if ($i ~ /^[0-9]+\.$/) { print int($i); exit }}')
        wpctl set-default "$sink"
      '')
    ];
    services.usbmuxd.enable = true; # for iOS mounting as storage
  };
}
