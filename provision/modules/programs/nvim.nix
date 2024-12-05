{ config, lib, pkgs, user, home-manager, ... }:

let cfg = config.modules.programs.nvim;
in {
  options.modules.programs.nvim = with lib; {
    enable = lib.mkOption {
      type = with types; bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      neovim # Fork of Vim aiming to improve extensibility and usability
      pyright # Latest version of the Pyright package, a static type checker for Python
    ];
  };
}











