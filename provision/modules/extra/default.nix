{ lib, ... }:
{
  imports = [
    ../programs/beancount.nix
  ];
  options.modules.extra.enable = lib.mkEnableOption "extra";
}
