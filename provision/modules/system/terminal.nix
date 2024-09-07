{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.terminal;
in {
  options.modules.system.terminal.enable = lib.mkEnableOption "terminal";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      git # Version control system for tracking changes in source code during software development.
      git-annex # Manages files with git, without checking the file contents into git.
      lazygit # Terminal-based GUI for git, making it easier to use and visualize git repositories.
      killall # Command-line utility to terminate processes by name.
      pciutils # Utilities for inspecting and manipulating devices connected to the PCI bus.
      chezmoi # Manages your dotfiles across multiple machines, ensuring consistency and version control.
      btop # Terminal-based resource monitor, providing an interactive view of system resources.
      nix-search-cli # Command-line utility for searching the Nix package repository.
      rbw # Command-line interface to the Bitwarden password manager.
      pinentry-curses # Simple curses-based passphrase entry dialog for GnuPG.
      bash # GNU Bourne-Again SHell, a command language interpreter for Unix-like operating systems.
      bash-completion # Provides programmable completion for the bash shell.
      tmux # Terminal multiplexer, allowing multiple terminal sessions within a single window.
      tmuxp # Manages tmux sessions through simple, declarative configuration files.
      thefuck # Corrects errors in previous console commands.
      nnn # Terminal file manager with a focus on performance and ease of use.
      advcpmv # Advanced version of the Unix utilities cp and mv.
      unzip # Command-line utility for extracting files from ZIP archives.
      trash-cli # Command-line interface to the freedesktop.org Trash.
      catimg # Display images in the terminal using ASCII characters.
      vim # Text editor that is highly configurable and widely used, especially in the Unix environment.
      neovim # Fork of Vim aiming to improve extensibility and usability.
      nodejs # JavaScript runtime built on Chrome's V8 JavaScript engine.
      ripgrep # Line-oriented search tool that recursively searches directories for a regex pattern.
      cargo # Package manager and build system for Rust.
      taskwarrior # Command-line task management tool.
      taskopen # Open Taskwarrior tasks in a text editor.
      taskwarrior-tui # Interactive terminal user interface for Taskwarrior.
      timewarrior # Command-line time tracking utility.
      ollama # Command-line tool for viewing and managing Open Location Codes.
      docker-compose
      opensc
      pcsc-tools
      firefox
      pkcs11helper
    ];
  };
}
