{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.terminal;
in {
  options.modules.system.terminal.enable = lib.mkEnableOption "terminal";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      git
      git-annex
      killall
      pciutils
      chezmoi

      rbw 
      # for rbw
        pinentry-curses 

      bash
      # for bash
        bash-completion
        tmux
        tmuxp

      nnn
      # for nnn
        advcpmv
        unzip
        trash-cli

      neovim
      # for neovim
        nodejs 
        ripgrep
    ];
  };
}
