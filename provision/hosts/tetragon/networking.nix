{ ... }:
{
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [
    53
    67
    68
  ];

  networking = {
    useDHCP = false;
    defaultGateway = "69.69.1.1";
    nameservers = [
      "9.9.9.9"
      "8.8.8.8"
      "1.1.1.1"
    ];

    interfaces = {
      enp1s0f0.useDHCP = false;
      enp1s0f0 = {
        ipv4.addresses = [
          {
            address = "69.69.1.10";
            prefixLength = 24;
          }
        ];
      };
    };
  };

  services.dnsmasq = {
    enable = true;
    settings = {
      server = [
        "9.9.9.9"
        "8.8.8.8"
        "1.1.1.1"
      ];
      domain-needed = true;
      bogus-priv = true;
      no-resolv = true;
      cache-size = 1000;

      dhcp-range = [ "enp1s0f0,69.69.1.50,69.69.1.254,24h" ];
      interface = "enp1s0f0";
      dhcp-host = "69.69.1.10";

      local = "/lan/";
      domain = "lan";
      expand-hosts = true;

      no-hosts = true;
      address = [ "/tetragon.lan/69.69.1.10" ];
      dhcp-option = [ "option:router,69.69.1.1" ];
    };
  };
}
