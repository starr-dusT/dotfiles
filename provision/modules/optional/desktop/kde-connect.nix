{ config, lib, pkgs, user, inputs, ... }:

let cfg = config.modules.desktop.gnome;
in {
  config = lib.mkIf cfg.enable {
    networking.firewall = rec {
      allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
      allowedUDPPortRanges = allowedTCPPortRanges;
    };

    programs.kdeconnect = {
      enable = true;
      package = pkgs.valent; # Implementation of the KDE Connect protocol, built on GNOME platform libraries
    };
  };
}
