{ config, lib, ... }:

let cfg = config.modules.optional.desktop.gnome-remote-desktop;
in {
  options.modules.optional.desktop.gnome-remote-desktop.enable = lib.mkEnableOption "gnome-remote-desktop";

  config = lib.mkIf cfg.enable {
    services.gnome.gnome-remote-desktop.enable = true;
    networking.firewall.allowedTCPPorts = [ 3389 ];
    
    # Ensure the service starts automatically at boot so the settings panel appears
    systemd.services.gnome-remote-desktop = {
      wantedBy = [ "graphical.target" ];
    };
    
    # Disable autologin to avoid session conflicts
    services.displayManager.autoLogin.enable = false;
    services.getty.autologinUser = null;
  };
}
