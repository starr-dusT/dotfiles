{ lib, ... }:
{
  imports = [
    ../../programs/beancount.nix
    ../../programs/borg.nix
    ../../programs/yt-dlp.nix
  ];
  options.modules.base-plus.enable = lib.mkEnableOption "base-plus";
}
