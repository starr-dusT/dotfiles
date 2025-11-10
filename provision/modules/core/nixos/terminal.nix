{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    bash # GNU Bourne-Again SHell, a command language interpreter for Unix-like operating systems
    bash-completion # Provides programmable completion for the bash shell
    tmux # Terminal multiplexer, allowing multiple terminal sessions within a single window
    killall # Command-line utility to terminate processes by name
    pciutils # Utilities for inspecting and manipulating devices connected to the PCI bus
    unzip # Command-line utility for extracting files from ZIP archives
    vim # Text editor that is highly configurable and widely used, especially in the Unix environment
    sesh # Smart session manager for the terminal
    just # Hand way to save and run project-specific commands
    zoxide # Fast cd command that learns your habits
    ripgrep # Line-oriented search tool that recursively searches directories for a regex pattern
    fzf # Command-line fuzzy finder for Unix-like operating systems
    nix-search-cli # Command-line utility for searching the Nix package repository
    trash-cli # Command-line interface to the freedesktop.org Trash
    btop # Terminal-based resource monitor, providing an interactive view of system resources
    nnn # Terminal file manager with a focus on performance and ease of use
    dysk # A linux utility listing your filesystems
    wl-clipboard # Command-line copy/paste utilities for Wayland
  ];
}
