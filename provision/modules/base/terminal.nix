{ config, lib, pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    bash # GNU Bourne-Again SHell, a command language interpreter for Unix-like operating systems
    bash-completion # Provides programmable completion for the bash shell
    tmux # Terminal multiplexer, allowing multiple terminal sessions within a single window
    killall # Command-line utility to terminate processes by name
    pciutils # Utilities for inspecting and manipulating devices connected to the PCI bus
    pinentry-curses # Simple curses-based passphrase entry dialog for GnuPG
    unzip # Command-line utility for extracting files from ZIP archives
    vim # Text editor that is highly configurable and widely used, especially in the Unix environment

    rbw # Command-line interface to the Bitwarden password manager
    advcpmv # Advanced version of the Unix utilities cp and mv
    trash-cli # Command-line interface to the freedesktop.org Trash
    ripgrep # Line-oriented search tool that recursively searches directories for a regex pattern
    sesh # Smart session manager for the terminal
    fzf # Command-line fuzzy finder for Unix-like operating systems
    nix-search-cli # Command-line utility for searching the Nix package repository

    btop # Terminal-based resource monitor, providing an interactive view of system resources
    nnn # Terminal file manager with a focus on performance and ease of use
  ];
}
