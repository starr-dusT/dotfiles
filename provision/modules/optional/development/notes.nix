{ config, lib, pkgs, ... }:

let cfg = config.modules.optional.development.notes;
in {
  options.modules.optional.development.notes.enable = lib.mkEnableOption "notes";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      obsidian # Note-taking and knowledge management application
      pandoc # Universal document converter
    ];
  };
}
