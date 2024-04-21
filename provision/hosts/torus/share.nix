{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 5001 ];
  networking.firewall.allowedUDPPorts = [ 5001 ];

  environment.systemPackages = with pkgs; [ 
    dufs # Distinctive utility file server
  ];

  systemd.services.share = {
    description = "Start dufs for quick sharing of files";
    wantedBy = [ "default.target" ];
    
    restartIfChanged = true;
    
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      ExecStart = "${pkgs.dufs}/bin/dufs -p 5001 -A /engi/apps/dufs/share";
    };
  };
}
