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
      yt-dlp # Command-line tool to download videos
      (pkgs.writeScriptBin "ytd_audio" ''
        #!/usr/bin/env bash
        yt-dlp -x \
        -ciw -o "%(title)s.%(ext)s" \
        --embed-thumbnail -S acodec:m4a "$1"
      '')
    ];
  };
}
