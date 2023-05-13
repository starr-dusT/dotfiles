# emacs because my life isn't already hard enough

{ config, lib, pkgs, user, ... }:

let cfg = config.modules.editors.emacs;
in {
  options.modules.editors.emacs.enable = lib.mkEnableOption "emacs";
  config = lib.mkIf cfg.enable {

    # Install packages
    environment.systemPackages = with pkgs; [ emacs silver-searcher ripgrep ];

    # Setup SystemCrafter's awesome crafted-emacs
    system.userActivationScripts = {
      installCraftedEmacs = ''
        if [ ! -d "/home/${user}/.emacs.d" ]; then
           ${pkgs.git}/bin/git clone "https://github.com/SystemCrafters/crafted-emacs.git" "/home/${user}/.emacs.d"
        fi
      '';
    };
  };
}
