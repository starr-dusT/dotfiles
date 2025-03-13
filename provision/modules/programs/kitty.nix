{ config, lib, pkgs, user, inputs, home-manager, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = with pkgs; [
        nerd-fonts.jetbrains-mono
      ];

      programs.kitty = {
        enable = true;
        themeFile = "gruvbox-dark";
        font.name = "JetBrainsMono Nerd Font";
        shellIntegration.mode = "no-cursor";
        settings = {
          disable_ligatures = "never";
          cursor_shape = "block";
          linux_display_server = "x11";
        };
      };
    };
  };
}
