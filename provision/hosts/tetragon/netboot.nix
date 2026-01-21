{ user, ... }:
let
  cfldr = "/podman/netbootxyz";
in
{
  networking.firewall.allowedTCPPorts = [ 3000 ];
  networking.firewall.allowedUDPPorts = [
    69
    3000
  ];

  systemd.tmpfiles.rules = [
    "d ${cfldr}/config 0775 ${user} users -"
    "d ${cfldr}/assets 0775 ${user} users -"
  ];

  virtualisation.oci-containers.containers."netbootxyz" = {
    autoStart = true;
    image = "ghcr.io/netbootxyz/netbootxyz";
    volumes = [
      "${cfldr}:/config"
      "${cfldr}:/assets"
    ];
    ports = [
      "69:69/udp"
      "3000:3000"
    ];
    extraOptions = [ "--pull=always" ];
  };
}
