{ config, lib, pkgs, user, inputs, home-manager, ... }:

let cfg = config.modules.programs.kitty;
in {
  options.modules.programs.kitty.enable = lib.mkEnableOption "kitty";
  config = lib.mkIf cfg.enable {
    home-manager.users.${user} = {
      home.packages = with pkgs; [
        nerdfonts
      ];

      programs.kitty = {
        enable = true;
        themeFile = "GruvboxMaterialDarkHard";
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
