{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.notes;
in {
  options.modules.devel.notes.enable = lib.mkEnableOption "notes";
  config = lib.mkIf cfg.enable {

  # Needed for obsidian
  nixpkgs.config.permittedInsecurePackages = [
    "electron-24.8.6"
  ];

    environment.systemPackages = with pkgs; [
      pandoc
      gollum
      obsidian

      zk
      # for zk
        bat
        fzf
    ];
  };
}
