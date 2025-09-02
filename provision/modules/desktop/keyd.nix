{ config, lib, pkgs, user, inputs, ... }:

let cfg = config.modules.desktop.gnome;
in {
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
            f2 = "oneshot(util)";
          };
          open = { # open applications on gnome bar
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
            b = "C-M-b"; # Set subwoofer to -9dB
            t = "C-M-t"; # Set subwoofer to -20dB
            l = "C-M-l"; # Set audio source to living room speakers
            d = "C-M-d"; # Set audio source to desk speakers/headphones
            k = "C-M-k"; # Set monitors for kestrel in living room
            s = "C-M-s"; # Set monitors for stormwalker in living room 
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
