{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.terminal;
in {
  options.modules.system.terminal.enable = lib.mkEnableOption "terminal";
  config = lib.mkIf cfg.enable {
    # Install packages
    environment.systemPackages = with pkgs; [
      bash
      bash-completion
      neovim
      ripgrep
      tmux
      tmuxp
      git
      git-annex
      killall
      pciutils
      pinentry-curses 
      trash-cli
      unzip
      nnn
      advcpmv
    ];
  };
}
