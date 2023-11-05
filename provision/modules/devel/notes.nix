{ config, lib, pkgs, pkgs-unstable, user, ... }:

let
  cfg = config.modules.devel.notes;
in {
  options.modules.devel.notes.enable = lib.mkEnableOption "notes";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      pandoc
      gollum
      zk
      # for zk
        bat
        fzf
    ] ++ [
      pkgs-unstable.obsidian
    ];
  };
}
