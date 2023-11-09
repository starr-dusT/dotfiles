{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 5000 ];
  networking.firewall.allowedUDPPorts = [ 5000 ];

  environment.systemPackages = with pkgs; [ dufs ];

  systemd.services.obsidian-vault = {
    description = "Start dufs containing obsidian vault (vulcan)";
    wantedBy = [ "default.target" ];
    
    restartIfChanged = true;
    
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      ExecStart = "${pkgs.dufs}/bin/dufs -p 5000 -A /engi/apps/dufs/vault";
    };
  };
}
