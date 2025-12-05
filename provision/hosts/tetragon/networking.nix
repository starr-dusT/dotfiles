{ ... }:
{
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  networking.firewall.allowedUDPPorts = [
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
    interfaces.enp1s0f0 = {
      useDHCP = false;
      ipv4.addresses = [
        {
          address = "69.69.1.10";
          prefixLength = 24;
        }
      ];
    };
  };

  services.dnsmasq = {
    enable = true;
    settings = {
      interface = "enp1s0f0";
      domain-needed = true;
      bogus-priv = true;
      no-resolv = true;
      cache-size = 1000;
      port = 5353; # Adguard is primary dns and fowards to this port
      server = [
        "9.9.9.9"
        "8.8.8.8"
        "1.1.1.1"
      ];

      dhcp-range = [ "enp1s0f0,69.69.1.50,69.69.1.240,24h" ];
      dhcp-option = [
        "6,69.69.1.10"
        "option:router,69.69.1.1"
      ];
      # PXE boot options
      dhcp-match = "set:efi64,60,PXEClient:Arch:00007";
      dhcp-boot = "tag:efi64,netboot.xyz.efi,,69.69.1.10";
      # Reserve IP by MAC/hostname
      dhcp-host = [
        "kestrel,69.69.1.143"
        "kestrel,69.69.1.184"
      ];

      # Dynamic dns by hostname
      local = "/lan/";
      domain = "lan";
      expand-hosts = true;
      no-hosts = true;
      # static dns entries
      address = [
        "/tetragon.lan/69.69.1.10"
        "/tv.lan/69.69.1.87"
        "/router.lan/69.69.1.1"
        "/vortex-1.lan/69.69.1.11"
        "/vortex-2.lan/69.69.1.12"
        "/vortex-3.lan/69.69.1.13"
        "/torus.lan/69.69.1.14"
      ];
    };
  };
}
