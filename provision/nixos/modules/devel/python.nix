# python with all the venom

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.python;
in {
  options.modules.devel.python.enable = lib.mkEnableOption "python";
  config = lib.mkIf cfg.enable {

    # Install packages
    environment.systemPackages = with pkgs; [ python3 python3Packages.virtualenv beancount fava ];
  };
}