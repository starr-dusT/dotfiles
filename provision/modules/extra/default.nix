{ lib, ... }:
{
  imports = [
    ../programs/beancount.nix
    ../programs/borg.nix
  ];
  options.modules.extra.enable = lib.mkEnableOption "extra";
}
