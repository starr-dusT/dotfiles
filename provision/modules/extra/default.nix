{ lib, ... }:
{
  imports = [
    ../programs/beancount.nix
    ../programs/borg.nix
    ../programs/yt-dlp.nix
  ];
  options.modules.extra.enable = lib.mkEnableOption "extra";
}
