# coding stuff for all the languages

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.tooling;
in {
  options.modules.devel.tooling.enable = lib.mkEnableOption "tooling";
  config = lib.mkIf cfg.enable {

    # Install packages
    environment.systemPackages = with pkgs; [
      neovim
      ripgrep
      tmux
      tmuxp
    ];
  };
}
