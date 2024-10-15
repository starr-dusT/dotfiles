{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.yt-dlp;
in {
  options.modules.programs.yt-dlp = with lib; {
    enable = lib.mkOption {
      type = with types; bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      yt-dlp
      (pkgs.writeScriptBin "ytd_audio" ''
        #!/usr/bin/env bash
        linux-mount-engi
        yt-dlp -x \
        -ciw -o "%(title)s.%(ext)s" \
        --embed-thumbnail -S acodec:m4a "$1"
      '')
    ];
  };
}
