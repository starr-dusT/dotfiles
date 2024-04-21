{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.notes;
in {
  options.modules.devel.notes.enable = lib.mkEnableOption "notes";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      pandoc # Universal document converter.
      gollum # Wiki software that provides a simple, Git-based wiki engine.
      obsidian # Note-taking and knowledge management application.
      zk # Command-line tool for interacting with Apache ZooKeeper, a centralized service for distributed systems.
      bat # Cat clone with syntax highlighting and Git integration.
      fzf # Command-line fuzzy finder for Unix-like operating systems.
    ];
  };
}
