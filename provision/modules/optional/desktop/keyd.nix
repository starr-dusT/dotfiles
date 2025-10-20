{ config, lib, ... }:

let
  cfg = config.modules.optional.desktop.gnome;
in
{
  config = lib.mkIf cfg.enable {
    services.keyd = {
      enable = true;
      keyboards.true = {
        ids = [ "*" ];
        settings = {
          main = {
            # Replace capslock with esc
            capslock = "overload(meta, esc)";

            # "Home" row modifiers for left modifier keys
            i = "overloadt(control, i, 300)";
            o = "overloadt(meta, o, 300)";
            p = "overloadt(alt, p, 300)";
          };
          meta = {
            o = "oneshot(open)";
            u = "oneshot(util)";
          };
          open = {
            # Open applications on gnome bar
            q = "C-M-1";
            w = "C-M-2";
            e = "C-M-3";
            r = "C-M-4";
            t = "C-M-5";
            y = "C-M-6";
            u = "C-M-7";
            i = "C-M-8";
            o = "C-M-9";
          };
          util = {
            d = "C-M-d"; # Kestrel display at desk
            l = "C-M-l"; # Kestrel display in living room
          };
          alt = {
            # Use hjkl for arrow keys with alt
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
