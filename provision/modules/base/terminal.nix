{ config, lib, pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    killall # Command-line utility to terminate processes by name
    pciutils # Utilities for inspecting and manipulating devices connected to the PCI bus
    btop # Terminal-based resource monitor, providing an interactive view of system resources
    nix-search-cli # Command-line utility for searching the Nix package repository
    rbw # Command-line interface to the Bitwarden password manager
    pinentry-curses # Simple curses-based passphrase entry dialog for GnuPG
    bash # GNU Bourne-Again SHell, a command language interpreter for Unix-like operating systems
    bash-completion # Provides programmable completion for the bash shell
    tmux # Terminal multiplexer, allowing multiple terminal sessions within a single window
    nnn # Terminal file manager with a focus on performance and ease of use
    advcpmv # Advanced version of the Unix utilities cp and mv
    unzip # Command-line utility for extracting files from ZIP archives
    trash-cli # Command-line interface to the freedesktop.org Trash
    vim # Text editor that is highly configurable and widely used, especially in the Unix environment
    ripgrep # Line-oriented search tool that recursively searches directories for a regex pattern
    sesh # Smart session manager for the terminal
    zoxide # Fast cd command that learns your habits
    fzf # Command-line fuzzy finder for Unix-like operating systems
  ];
}
