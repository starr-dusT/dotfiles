{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      libnotify # Library that sends desktop notifications to a notification daemon
      rofi # Window switcher, run dialog and dmenu replacement
    ];
  };
}
