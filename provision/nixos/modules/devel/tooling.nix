# coding stuff for all the languages

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.tooling;
in {
  options.modules.devel.tooling.enable = lib.mkEnableOption "tooling";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      cmake
      gcc
      coreutils
      gnumake
    ];
  };
}
