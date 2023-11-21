{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.kitty;
in {
  options.modules.desktop.kitty.enable = lib.mkEnableOption "kitty";
  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      theme = "Dracula";
      font.name = "JetBrainsMono Nerd Font";
      shellIntegration.mode = "no-cursor";
      settings = {
        disable_ligatures = "never";
        cursor_shape = "block";
      };
    };
  };
}
