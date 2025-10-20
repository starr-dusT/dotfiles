{ config, lib, pkgs, user, ... }:

let cfg = config.modules.devel.notes;
in {
  options.modules.devel.notes.enable = lib.mkEnableOption "notes";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      obsidian # Note-taking and knowledge management application
      pandoc # Universal document converter
    ];
  };
}
