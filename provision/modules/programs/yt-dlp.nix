{ config, lib, pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    yt-dlp # Command-line tool to download videos
    (pkgs.writeScriptBin "ytd_audio" ''
      #!/usr/bin/env bash
      yt-dlp -x \
      -ciw -o "%(title)s.%(ext)s" \
      --embed-thumbnail -S acodec:m4a "$1"
    '')
  ];
}
