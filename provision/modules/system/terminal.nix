{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.terminal;
in {
  options.modules.system.terminal.enable = lib.mkEnableOption "terminal";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      killall # Command-line utility to terminate processes by name.
      pciutils # Utilities for inspecting and manipulating devices connected to the PCI bus.
      btop # Terminal-based resource monitor, providing an interactive view of system resources.
      nix-search-cli # Command-line utility for searching the Nix package repository.
      rbw # Command-line interface to the Bitwarden password manager.
      pinentry-curses # Simple curses-based passphrase entry dialog for GnuPG.
      bash # GNU Bourne-Again SHell, a command language interpreter for Unix-like operating systems.
      bash-completion # Provides programmable completion for the bash shell.
      tmux # Terminal multiplexer, allowing multiple terminal sessions within a single window.
      thefuck # Corrects errors in previous console commands.
      nnn # Terminal file manager with a focus on performance and ease of use.
      advcpmv # Advanced version of the Unix utilities cp and mv.
      unzip # Command-line utility for extracting files from ZIP archives.
      trash-cli # Command-line interface to the freedesktop.org Trash.
      vim # Text editor that is highly configurable and widely used, especially in the Unix environment.
      neovim # Fork of Vim aiming to improve extensibility and usability.
      nodejs # JavaScript runtime built on Chrome's V8 JavaScript engine.
      ripgrep # Line-oriented search tool that recursively searches directories for a regex pattern.
      cargo # Package manager and build system for Rust.
      docker-compose
      sesh
      zoxide
    ];
  };
}
