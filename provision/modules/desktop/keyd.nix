{ config, lib, pkgs, user, inputs, ... }:
let
  cfg = config.modules.desktop.gnome;
in {
  # Only launch for Gnome
  # TODO: if using with other WMs/DEs revise this.
  config = lib.mkIf cfg.enable {
    services.keyd = {
      enable = true;
      keyboards.true = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "overload(meta, esc)";
            leftalt = "layer(alt)";
            f1 = "oneshot(open)";
          };
          open = {
            enter = "C-M-enter"; # terminal
            b = "C-M-b";         # browser
            s = "C-M-s";         # steam
            d = "C-M-d";         # discord
            f = "C-M-f";         # nautilus
          };
          alt = {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
          };
        };
      };
    };
  };
}
