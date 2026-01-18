{ user, ... }:
let
  cfldr = "/home/${user}/.podman/databasus";
in
{
  networking.firewall.allowedTCPPorts = [ 4005 ];
  networking.firewall.allowedUDPPorts = [ 4005 ];

  systemd.tmpfiles.rules = [
    "d ${cfldr} 0775 ${user} users -"
  ];

  virtualisation.oci-containers.containers."databasus" = {
    autoStart = true;
    image = "databasus/databasus:latest";
    volumes = [
      "${cfldr}:/databasus-data"
    ];
    ports = [
      "4005:4005"
    ];
    extraOptions = [ "--pull=always" ];
  };
}
