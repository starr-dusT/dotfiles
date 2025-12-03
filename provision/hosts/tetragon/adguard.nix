{ ... }:
{
  networking.firewall.allowedTCPPorts = [
    3003
    53
  ];
  networking.firewall.allowedUDPPorts = [
    3003
    53
  ];

  services.adguardhome = {
    enable = true;
    host = "0.0.0.0";
    port = 3003;
    settings = {
      dns = {
        upstream_dns = [
          "127.0.0.1:5353"
        ];
      };
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
      };
    };
  };
}
