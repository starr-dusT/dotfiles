# stow config files for great sanity!

{ config, lib, pkgs, user, ... }:

let cfg = config.modules.stow;
in {
  options.modules.stow.enable = lib.mkEnableOption "stow";
  config = lib.mkIf cfg.enable {

    # Install packages
    environment.systemPackages = with pkgs; [ stow ];

    # Run script that checks the .stow-on-rebuild file
    # if it is 1 then it will re-stow else it won't
    # this is to aviod the slow stow on every rebuild
    system.userActivationScripts = {
     stowDots = ''
       if [ -f "/home/${user}/.stow-on-rebuild" ]; then
         if [ $(cat "/home/${user}/.stow-on-rebuild") -eq 1 ]; then
           cd "/home/${user}/.setup/local/stow"
           ${pkgs.stow}/bin/stow . -t "/home/${user}" --no-folding
         fi
       fi
     '';
    };
  };
}
